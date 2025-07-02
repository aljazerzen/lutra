use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct IdGenerator<T: From<usize> = usize> {
    next_id: usize,
    phantom: PhantomData<T>,
}

impl<T: From<usize>> IdGenerator<T> {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::default()
    }

    #[allow(dead_code)]
    pub fn new_at(x: usize) -> Self {
        IdGenerator {
            next_id: x + 1,
            phantom: PhantomData,
        }
    }

    #[allow(dead_code)]
    pub fn skip(&mut self, id: usize) {
        self.next_id = self.next_id.max(id + 1);
    }

    pub fn next(&mut self) -> T {
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

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct NameGenerator {
    id_generator: IdGenerator,
    prefix: &'static str,
}

impl NameGenerator {
    #[allow(dead_code)]
    pub fn new(prefix: &'static str) -> Self {
        Self {
            id_generator: IdGenerator::new(),
            prefix,
        }
    }

    #[allow(dead_code)]
    pub fn next(&mut self) -> String {
        // if self.prefix == "r" && self.id_generator.next_id == 3 {
        // panic!();
        // }
        format!("{}{}", self.prefix, self.id_generator.next())
    }
}
