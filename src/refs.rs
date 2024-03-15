enum Shared {}

enum Exclusive {}

trait RefKind {
    type Ref<'a, T: 'a>;
}

impl RefKind for Shared {
    type Ref<'a, T: 'a> = &'a T;
}

impl RefKind for Exclusive {
    type Ref<'a, T: 'a> = &'a mut T;
}

type Demo<'a> = DemoImpl<'a, Shared>;

type DemoMut<'a> = DemoImpl<'a, Exclusive>;

struct DemoImpl<'a, R: RefKind> {
    demo: R::Ref<'a, usize>,
}

impl DemoImpl<'_, Shared> {
    fn demo(&self) -> &usize {
        self.demo
    }
}

impl DemoImpl<'_, Exclusive> {
    fn demo_mut(&mut self) -> &mut usize {
        self.demo
    }
}
