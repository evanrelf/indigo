pub trait Reflect {
    type Value;

    fn reflect() -> Self::Value;
}
