pub trait PeekingNextIfEq: itertools::PeekingNext {
    fn peeking_next_if_eq(&mut self, v: Self::Item) -> Option<Self::Item>
    where
        Self::Item: PartialEq,
    {
        self.peeking_next(|p| *p == v)
    }
}

impl<T: itertools::PeekingNext> PeekingNextIfEq for T {}
