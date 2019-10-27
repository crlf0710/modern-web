pub trait IteratorAdvanceIf<I: Iterator> {
    fn advance_if(&mut self, f: impl FnOnce(&I::Item) -> bool) -> Option<I::Item>;

    fn advance_if_iter<F: FnMut(&I::Item) -> bool>(&mut self, f: F) -> IterAdvanceIf<'_, I, F>
    where
        Self: Sized;

    fn advance_if_eq(&mut self, v: I::Item) -> Option<I::Item>
    where
        I::Item: PartialEq,
    {
        self.advance_if(|p| *p == v)
    }
}

impl<I: Iterator> IteratorAdvanceIf<I> for std::iter::Peekable<I> {
    fn advance_if(&mut self, f: impl FnOnce(&I::Item) -> bool) -> Option<I::Item> {
        let peeked_next = self.peek()?;
        if (f)(peeked_next) {
            self.next()
        } else {
            None
        }
    }

    fn advance_if_iter<F: FnMut(&I::Item) -> bool>(&mut self, f: F) -> IterAdvanceIf<'_, I, F> {
        IterAdvanceIf { iter: self, f }
    }
}

pub struct IterAdvanceIf<'a, I, F>
where
    I: Iterator,
    F: FnMut(&I::Item) -> bool,
{
    iter: &'a mut std::iter::Peekable<I>,
    f: F,
}

impl<'a, I, F> Iterator for IterAdvanceIf<'a, I, F>
where
    I: Iterator,
    F: FnMut(&I::Item) -> bool,
{
    type Item = <I as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.advance_if(&mut self.f)
    }
}
