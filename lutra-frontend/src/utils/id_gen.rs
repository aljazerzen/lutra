use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct IdGenerator<T: From<usize>> {
    next_id: usize,
    phantom: PhantomData<T>,
}

impl<T: From<usize>> IdGenerator<T> {
    pub fn new() -> Self {
        Self::default()
    }

    #[allow(dead_code)]
    pub fn skip(&mut self, id: usize) {
        self.next_id = self.next_id.max(id + 1);
    }

    pub fn gen(&mut self) -> T {
        let id = self.next_id;
        self.next_id += 1;
        T::from(id)
    }
}

impl<T: From<usize>> Default for IdGenerator<T> {
    fn default() -> IdGenerator<T> {
        IdGenerator {
            next_id: 0,
            phantom: PhantomData,
        }
    }
}
