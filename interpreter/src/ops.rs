use std::ops::Add;
use std::marker::Sized;

pub trait CheckedAdd: Add<Self, Output=Self> where Self: Sized {
    fn checked_add(self, v: Self) -> Option<Self>;
}

pub trait PtrEq<Rhs = Self>  {
    fn identical(&self, other: &Rhs) -> bool;
}

impl CheckedAdd for i64 {
    fn checked_add(self, v: Self) -> Option<Self> {
        self.checked_add(v)
    }
}
