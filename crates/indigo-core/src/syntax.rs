use ropey::Rope;
use std::ops::Deref;
use tree_sitter::{Parser, Tree};

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
