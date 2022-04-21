use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

pub type TypeMap = HashMap<TypeId, Box<dyn Any>>;
