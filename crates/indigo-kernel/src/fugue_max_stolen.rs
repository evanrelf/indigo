// https://github.com/josephg/crdt-from-scratch/blob/7688f143/crdt.ts

#![allow(clippy::needless_return)]
#![allow(clippy::partialeq_to_none)]
#![allow(clippy::pedantic)]
#![allow(unused)]

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
struct Id {
    agent: String,
    seq: usize,
}

#[derive(Clone, Debug, PartialEq)]
struct Item {
    content: char,
    id: Id,
    origin_left: Option<Id>,
    origin_right: Option<Id>,
    deleted: bool,
}

struct Version(HashMap<String, usize>);

struct Doc {
    content: Vec<Item>,
    version: Version,
}

fn create_doc() -> Doc {
    Doc {
        content: Vec::new(),
        version: Version(HashMap::new()),
    }
}

fn get_content(doc: &Doc) -> String {
    let mut content = String::new();
    for item in &doc.content {
        if !item.deleted {
            content.push(item.content);
        }
    }
    return content;
}

// Find the index of the item at the specified content position in the document.
fn find_item_at_pos(doc: &Doc, mut pos: usize, stick_end: bool) -> usize {
    let mut i = 0;
    while i < doc.content.len() {
        let item = &doc.content[i];
        if stick_end && pos == 0 {
            return i;
        } else if item.deleted {
            i += 1;
            continue;
        } else if pos == 0 {
            return i;
        } else {
            pos -= 1;
            i += 1;
        }
    }
    if pos == 0 {
        return i;
    } else {
        panic!("past end of the document")
    }
}

fn local_insert_one(doc: &mut Doc, agent: &str, pos: usize, text: char) {
    let idx = find_item_at_pos(doc, pos, true);
    let seq = doc.version.0.get(agent).map_or(0, |seq| seq + 1);
    integrate(
        doc,
        &Item {
            content: text,
            id: Id {
                agent: agent.to_owned(),
                seq,
            },
            deleted: false,
            origin_left: if idx > 0 {
                doc.content.get(idx - 1).map(|item| item.id.clone())
            } else {
                None
            },
            origin_right: doc.content.get(idx).map(|item| item.id.clone()),
        },
    );
}

fn local_insert(doc: &mut Doc, agent: &str, mut pos: usize, text: &str) {
    for c in text.chars() {
        local_insert_one(doc, agent, pos, c);
        pos += 1;
    }
}

fn remote_insert(doc: &mut Doc, item: &Item) {
    integrate(doc, item);
}

fn local_delete(doc: &mut Doc, pos: usize, mut del_len: usize) {
    while del_len > 0 {
        let idx = find_item_at_pos(doc, pos, false);
        doc.content[idx].deleted = true;
        del_len -= 1;
    }
}

fn find_item_idx_at_id(doc: &Doc, id: Option<&Id>) -> Option<usize> {
    let id = id?;
    for i in 0..doc.content.len() {
        if doc.content[i].id == *id {
            return Some(i);
        }
    }
    panic!("Can't find item");
}

fn integrate(doc: &mut Doc, new_item: &Item) {
    let Id { agent, seq } = &new_item.id;
    let last_seen = doc.version.0.get(agent);
    if *seq != last_seen.map_or(0, |n| n + 1) {
        panic!("Operations out of order");
    }
    // Mark the item in the document version.
    doc.version.0.insert(agent.to_owned(), *seq);
    // If originLeft is null, that means it was inserted at the start of the document.
    // We'll pretend there was some item at position -1 which we were inserted to the
    // right of.
    let left = find_item_idx_at_id(doc, new_item.origin_left.as_ref());
    let mut dest_idx = left.map_or(0, |n| n + 1);
    let right = if new_item.origin_right == None {
        doc.content.len()
    } else {
        find_item_idx_at_id(doc, new_item.origin_right.as_ref()).unwrap()
    };
    let mut scanning = false;
    // This loop scans forward from destIdx until it finds the right place to insert into
    // the list.
    let mut i = dest_idx;
    loop {
        if !scanning {
            dest_idx = i;
        }
        // If we reach the end of the document, just insert.
        if i == doc.content.len() {
            break;
        }
        // No ambiguity / concurrency. Insert here.
        if i == right {
            break;
        }
        let other = &doc.content[i];
        let oleft = find_item_idx_at_id(doc, other.origin_left.as_ref());
        let oright = if other.origin_right == None {
            doc.content.len()
        } else {
            find_item_idx_at_id(doc, other.origin_right.as_ref()).unwrap()
        };
        if oleft < left || (oleft == left && oright == right && new_item.id.agent < other.id.agent)
        {
            break;
        }
        if oleft == left {
            scanning = oright < right;
        }
        i += 1;
    }
    // We've found the position. Insert here.
    doc.content.insert(dest_idx, new_item.clone());
}

fn is_in_version(id: Option<&Id>, version: &Version) -> bool {
    if id == None {
        return true;
    }
    let Id { agent, seq } = id.unwrap();
    let highest_seq = version.0.get(agent);
    if highest_seq == None {
        return false;
    } else {
        return highest_seq.unwrap() >= seq;
    }
}

fn can_insert_now(item: &Item, doc: &Doc) -> bool {
    // We need op.id to not be in doc.versions, but originLeft and originRight to be in.
    // We're also inserting each item from each agent in sequence.
    let Id { agent, seq } = &item.id;
    return !is_in_version(Some(&item.id), &doc.version)
        && (*seq == 0
            || is_in_version(
                Some(&Id {
                    agent: agent.to_owned(),
                    seq: seq - 1,
                }),
                &doc.version,
            ))
        && is_in_version(item.origin_left.as_ref(), &doc.version)
        && is_in_version(item.origin_right.as_ref(), &doc.version);
}

fn merge_into(dest: &mut Doc, src: &Doc) {
    let mut missing: Vec<Item> = src
        .content
        .iter()
        .filter(|item| !is_in_version(Some(&item.id), &dest.version))
        .cloned()
        .collect();
    let mut remaining = missing.len();
    while remaining > 0 {
        // Find the next item in remaining and insert it.
        let mut merged_on_this_pass = 0;
        for i in 0..missing.len() {
            let item = missing.get(i);
            if item == None {
                continue;
            }
            if !can_insert_now(item.unwrap(), dest) {
                continue;
            }
            // Insert it.
            remote_insert(dest, item.unwrap());
            missing.remove(i);
            remaining -= 1;
            merged_on_this_pass += 1;
        }
        if merged_on_this_pass == 0 {
            panic!("Not making progress");
        }
    }
    let mut src_idx = 0;
    let mut dest_idx = 0;
    while src_idx < src.content.len() {
        let src_item = &src.content[src_idx];
        let mut dest_item = &mut dest.content[dest_idx];
        while src_item.id != dest_item.id {
            dest_idx += 1;
            dest_item = &mut dest.content[dest_idx];
        }
        if src_item.deleted {
            dest_item.deleted = true;
        }
        src_idx += 1;
        dest_idx += 1;
    }
}

pub struct CrdtDocument {
    inner: Doc,
    agent: String,
}

impl CrdtDocument {
    pub fn new(agent: &str) -> Self {
        Self {
            inner: create_doc(),
            agent: agent.to_owned(),
        }
    }

    pub fn ins(&mut self, pos: usize, text: &str) {
        local_insert(&mut self.inner, &self.agent, pos, text);
    }

    pub fn del(&mut self, pos: usize, del_len: usize) {
        local_delete(&mut self.inner, pos, del_len);
    }

    pub fn get_string(&self) -> String {
        return get_content(&self.inner);
    }

    pub fn merge_from(&mut self, other: Self) {
        merge_into(&mut self.inner, &other.inner);
    }

    pub fn reset(&mut self) {
        self.inner = create_doc();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut doc1 = create_doc();
        let mut doc2 = create_doc();

        local_insert(&mut doc1, "a", 0, "A");
        local_insert(&mut doc2, "b", 0, "B");

        merge_into(&mut doc1, &doc2);
        merge_into(&mut doc2, &doc1);

        println!("doc1 has content {}", get_content(&doc1));
        println!("doc2 has content {}", get_content(&doc2));

        local_delete(&mut doc1, 0, 1);
        println!("doc1 has content {}", get_content(&doc1));

        merge_into(&mut doc2, &doc1);
        println!("doc2 has content {}", get_content(&doc2));

        println!("{:#?}", doc2.content);

        //

        local_insert_one(&mut doc1, "seph", 0, 'a');
        merge_into(&mut doc2, &doc1);

        local_insert_one(&mut doc1, "seph", 1, 'b');
        local_insert_one(&mut doc1, "seph", 0, 'c');
        println!("doc1 has content {}", get_content(&doc1));
        println!("{:#?}", doc1.content);

        merge_into(&mut doc2, &doc1);
        println!("doc2 has content {}", get_content(&doc2));

        println!("{:#?}", doc2.content);

        // panic!() // panic to show `println` output
    }
}
