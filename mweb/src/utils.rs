pub trait PeekingNextIfEq: itertools::PeekingNext {
    fn peeking_next_if_eq(&mut self, v: Self::Item) -> Option<Self::Item>
    where
        Self::Item: PartialEq,
    {
        self.peeking_next(|p| *p == v)
    }
}

impl<T: itertools::PeekingNext> PeekingNextIfEq for T {}

#[derive(Copy, Clone, Debug)]
pub struct SpanRef<'a, T> {
    pos: usize,
    slice: &'a [T],
}

impl<'a, T> SpanRef<'a, T> {
    pub fn new(slice: &'a [T]) -> Self {
        SpanRef { pos: 0, slice }
    }

    pub fn is_empty(self) -> bool {
        self.slice.is_empty()
    }

    pub fn len(self) -> usize {
        self.slice.len()
    }

    pub fn front_cloned(self) -> Option<T>
    where
        T: Clone,
    {
        self.slice.get(0).cloned()
    }

    pub fn pos(self) -> usize {
        self.pos
    }

    pub fn range(self, r: impl SpanRangeIndex<T>) -> SpanRef<'a, T> {
        use std::ops::Index;
        SpanRef {
            pos: self.pos + r.clone().delta_start(),
            slice: self.slice.index(r),
        }
    }

    pub fn split_at(self, pos: usize) -> (&'a [T], SpanRef<'a, T>) {
        let (before, after) = self.slice.split_at(pos);
        (
            before,
            SpanRef {
                pos: self.pos + pos,
                slice: after,
            },
        )
    }

    pub fn starts_with(self, needle: &[T]) -> bool
    where
        T: PartialEq,
    {
        self.slice.starts_with(needle)
    }
}

impl<'a> SpanRef<'a, u8> {
    pub fn bytes(self) -> &'a [u8] {
        self.slice
    }
}

use std::slice::SliceIndex;
pub trait SpanRangeIndex<T>: SliceIndex<[T], Output = [T]> + Clone {
    fn delta_start(self) -> usize;
}

mod span_range_index_impl {
    use super::SpanRangeIndex;
    use std::ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive};
    impl<T> SpanRangeIndex<T> for Range<usize> {
        fn delta_start(self) -> usize {
            self.start
        }
    }
    impl<T> SpanRangeIndex<T> for RangeFrom<usize> {
        fn delta_start(self) -> usize {
            self.start
        }
    }
    impl<T> SpanRangeIndex<T> for RangeInclusive<usize> {
        fn delta_start(self) -> usize {
            *self.start()
        }
    }
    impl<T> SpanRangeIndex<T> for RangeFull {
        fn delta_start(self) -> usize {
            0
        }
    }

    impl<T> SpanRangeIndex<T> for RangeTo<usize> {
        fn delta_start(self) -> usize {
            0
        }
    }

    impl<T> SpanRangeIndex<T> for RangeToInclusive<usize> {
        fn delta_start(self) -> usize {
            0
        }
    }
}

pub type U8SpanRef<'a> = SpanRef<'a, u8>;
