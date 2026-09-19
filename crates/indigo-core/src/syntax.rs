use anyhow::{Context as _, anyhow};
use camino::Utf8Path;
use ropey::Rope;
use std::{
    ops::{Deref, Range},
    str::FromStr,
};
use tree_sitter::{Node, Parser, Query, TextProvider, Tree};

#[allow(clippy::allow_attributes, unused_imports)]
use std::sync::OnceLock;

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
        // TODO: If I want to do incremental reparsing, I need to call `tree.edit(..)`. For now I'll
        // just parse from scratch every time.
        // self.tree = parse(code, &mut self.parser, Some(&self.tree));
        self.tree = parse(code, &mut self.parser, None);
    }

    #[must_use]
    pub fn language(&self) -> Language {
        self.language
    }

    #[must_use]
    pub fn tree(&self) -> &Tree {
        &self.tree
    }

    /// Nodes from the root down to the smallest node that covers `byte_range`.
    #[must_use]
    pub fn node_path(&self, byte_range: Range<usize>) -> Vec<Node<'_>> {
        let leaf = self
            .tree
            .root_node()
            .descendant_for_byte_range(byte_range.start, byte_range.end);
        let mut path = Vec::new();
        let mut node = leaf;
        while let Some(current) = node {
            path.push(current);
            node = current.parent();
        }
        path.reverse();
        path
    }

    /// First named node, in document order, whose byte range lies strictly inside `byte_range`.
    #[must_use]
    pub fn inner_node(&self, byte_range: Range<usize>) -> Option<Node<'_>> {
        let mut node = self
            .tree
            .root_node()
            .descendant_for_byte_range(byte_range.start, byte_range.end)?;
        loop {
            let mut cursor = node.walk();
            let child = node.named_children(&mut cursor).find(|child| {
                !child.byte_range().is_empty()
                    && byte_range.start <= child.start_byte()
                    && child.end_byte() <= byte_range.end
            })?;
            if child.byte_range() != byte_range {
                return Some(child);
            }
            node = child;
        }
    }

    /// Smallest node whose byte range strictly contains `byte_range`.
    ///
    /// The root node starts after any leading whitespace, so a range that begins in that
    /// whitespace has no outer node.
    #[must_use]
    pub fn outer_node(&self, byte_range: Range<usize>) -> Option<Node<'_>> {
        let mut node = self
            .tree
            .root_node()
            .descendant_for_byte_range(byte_range.start, byte_range.end)?;
        while !strictly_contains(&node.byte_range(), &byte_range) {
            node = node.parent()?;
        }
        Some(node)
    }
}

fn strictly_contains(outer: &Range<usize>, inner: &Range<usize>) -> bool {
    outer.start <= inner.start && inner.end <= outer.end && outer != inner
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

#[derive(Clone, Copy, Debug)]
pub enum Language {
    #[cfg(feature = "language-rust")]
    Rust,
}

impl Language {
    #[must_use]
    pub fn highlights_query(&self) -> &'static Query {
        match *self {
            #[cfg(feature = "language-rust")]
            Self::Rust => {
                static QUERY: OnceLock<Query> = OnceLock::new();
                QUERY.get_or_init(|| {
                    Query::new(&(*self).into(), tree_sitter_rust::HIGHLIGHTS_QUERY).unwrap()
                })
            }
        }
    }

    #[must_use]
    pub fn injections_query(&self) -> &'static Query {
        match *self {
            #[cfg(feature = "language-rust")]
            Self::Rust => {
                static QUERY: OnceLock<Query> = OnceLock::new();
                QUERY.get_or_init(|| {
                    Query::new(&(*self).into(), tree_sitter_rust::INJECTIONS_QUERY).unwrap()
                })
            }
        }
    }

    #[must_use]
    pub fn tags_query(&self) -> &'static Query {
        match *self {
            #[cfg(feature = "language-rust")]
            Self::Rust => {
                static QUERY: OnceLock<Query> = OnceLock::new();
                QUERY.get_or_init(|| {
                    Query::new(&(*self).into(), tree_sitter_rust::TAGS_QUERY).unwrap()
                })
            }
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

impl FromStr for Language {
    type Err = anyhow::Error;
    fn from_str(name: &str) -> Result<Self, Self::Err> {
        match name {
            #[cfg(feature = "language-rust")]
            "rust" => Ok(Self::Rust),
            _ => Err(anyhow!("unknown language `{name}`")),
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

    #[test]
    #[cfg_attr(
        not(feature = "language-rust"),
        ignore = "requires 'language-rust' feature"
    )]
    fn test_node_path() {
        #[cfg(feature = "language-rust")]
        {
            let rope = Rope::from("fn main() {}");
            let syntax = Syntax::parse(Language::Rust, &rope);
            let kinds = syntax
                .node_path(3..4)
                .iter()
                .map(Node::kind)
                .collect::<Vec<_>>();
            assert_eq!(kinds, ["source_file", "function_item", "identifier"]);
            let kinds = syntax
                .node_path(0..2)
                .iter()
                .map(Node::kind)
                .collect::<Vec<_>>();
            assert_eq!(kinds, ["source_file", "function_item", "fn"]);
        }
        #[cfg(not(feature = "language-rust"))]
        panic!("requires 'language-rust' feature");
    }

    #[test]
    #[cfg_attr(
        not(feature = "language-rust"),
        ignore = "requires 'language-rust' feature"
    )]
    fn test_outer_inner_node() {
        #[cfg(feature = "language-rust")]
        {
            let rope = Rope::from("fn main() {}\n");
            let syntax = Syntax::parse(Language::Rust, &rope);
            let outer = |range| {
                syntax
                    .outer_node(range)
                    .map(|node| (node.kind(), node.byte_range()))
            };
            let inner = |range| {
                syntax
                    .inner_node(range)
                    .map(|node| (node.kind(), node.byte_range()))
            };
            // `m` grows to `main`, which grows to the whole item.
            assert_eq!(outer(3..4), Some(("identifier", 3..7)));
            assert_eq!(outer(3..7), Some(("function_item", 0..12)));
            // The root has nothing above it.
            assert_eq!(outer(0..13), None);
            // The item shrinks to its first named child.
            assert_eq!(inner(0..12), Some(("identifier", 3..7)));
            // Leaves and partial slices of leaves have nothing inside them.
            assert_eq!(inner(3..7), None);
            assert_eq!(inner(3..5), None);
            // A selection covering no complete named node stays put.
            assert_eq!(inner(5..8), None);

            // The root node starts after leading whitespace.
            let rope = Rope::from("\nfn main() {}\n");
            let syntax = Syntax::parse(Language::Rust, &rope);
            assert_eq!(syntax.outer_node(0..1), None);
            assert_eq!(
                syntax
                    .outer_node(1..3)
                    .map(|node| (node.kind(), node.byte_range())),
                Some(("function_item", 1..13))
            );
        }
        #[cfg(not(feature = "language-rust"))]
        panic!("requires 'language-rust' feature");
    }
}
