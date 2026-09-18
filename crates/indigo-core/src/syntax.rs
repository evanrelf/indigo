use anyhow::{Context as _, anyhow};
use camino::Utf8Path;
use ropey::Rope;
use std::ops::Deref;
use tree_sitter::{Node, Parser, Query, TextProvider, Tree};

pub struct Syntax {
    language: Language,
    parser: Parser,
    tree: Tree,
}

impl Syntax {
    #[must_use]
    pub fn parse(language: Language, code: &Rope) -> Self {
        let mut parser = Parser::new();
        parser.set_language(&language.into()).unwrap();
        let tree = parse(code, &mut parser, None);
        Self {
            language,
            parser,
            tree,
        }
    }

    pub fn reparse(&mut self, code: &Rope) {
        self.tree = parse(code, &mut self.parser, Some(&self.tree));
    }

    #[must_use]
    pub fn language(&self) -> Language {
        self.language
    }

    #[must_use]
    pub fn tree(&self) -> &Tree {
        &self.tree
    }
}

impl Deref for Syntax {
    type Target = Tree;
    fn deref(&self) -> &Self::Target {
        &self.tree
    }
}

impl Clone for Syntax {
    fn clone(&self) -> Self {
        let mut parser = Parser::new();
        parser.set_language(&self.language.into()).unwrap();
        Self {
            language: self.language,
            parser,
            tree: self.tree.clone(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Language {
    #[cfg(feature = "language-rust")]
    Rust,
}

impl Language {
    #[must_use]
    pub fn highlights_query(&self) -> Query {
        match self {
            #[cfg(feature = "language-rust")]
            Self::Rust => Query::new(&(*self).into(), tree_sitter_rust::HIGHLIGHTS_QUERY).unwrap(),
        }
    }

    #[must_use]
    pub fn injections_query(&self) -> Query {
        match self {
            #[cfg(feature = "language-rust")]
            Self::Rust => Query::new(&(*self).into(), tree_sitter_rust::INJECTIONS_QUERY).unwrap(),
        }
    }

    #[must_use]
    pub fn tags_query(&self) -> Query {
        match self {
            #[cfg(feature = "language-rust")]
            Self::Rust => Query::new(&(*self).into(), tree_sitter_rust::TAGS_QUERY).unwrap(),
        }
    }
}

impl TryFrom<&Utf8Path> for Language {
    type Error = anyhow::Error;
    fn try_from(path: &Utf8Path) -> Result<Self, Self::Error> {
        let extension_str = path.extension().context("path is missing extension")?;
        match extension_str {
            #[cfg(feature = "language-rust")]
            "rs" => Ok(Self::Rust),
            _ => Err(anyhow!("could not infer language from path")),
        }
    }
}

impl From<Language> for tree_sitter::Language {
    fn from(language: Language) -> Self {
        match language {
            #[cfg(feature = "language-rust")]
            Language::Rust => tree_sitter_rust::LANGUAGE.into(),
        }
    }
}

fn parse(rope: &Rope, parser: &mut Parser, old_tree: Option<&Tree>) -> Tree {
    parser
        .parse_with_options(
            &mut |offset, _point| -> &[u8] {
                if offset >= rope.len() {
                    return &[];
                }
                let (chunk, chunk_start) = rope.chunk(offset);
                &chunk.as_bytes()[offset - chunk_start..]
            },
            old_tree,
            None,
        )
        .expect("parser has a language set")
}

pub struct RopeTextProvider<'a>(pub ropey::RopeSlice<'a>);

impl<'a> TextProvider<&'a [u8]> for RopeTextProvider<'a> {
    type I = std::iter::Map<ropey::iter::Chunks<'a>, fn(&'a str) -> &'a [u8]>;
    fn text(&mut self, node: Node) -> Self::I {
        self.0.slice(node.byte_range()).chunks().map(str::as_bytes)
    }
}

#[cfg(test)]
mod tests {
    #[allow(clippy::allow_attributes, unused_imports)]
    use super::*;

    #[test]
    #[cfg_attr(
        not(feature = "language-rust"),
        ignore = "requires 'language-rust' feature"
    )]
    fn test_rust() {
        #[cfg(feature = "language-rust")]
        {
            let rope = Rope::from("fn main() {}");
            let syntax = Syntax::parse(Language::Rust, &rope);
            let main_range = syntax
                .root_node()
                .child(0) // entire file
                .unwrap()
                .child(1) // `main`
                .unwrap()
                .range();
            assert_eq!(
                &rope
                    .slice(main_range.start_byte..main_range.end_byte)
                    .to_string(),
                "main"
            );
        }
        #[cfg(not(feature = "language-rust"))]
        panic!("requires 'language-rust' feature");
    }
}
