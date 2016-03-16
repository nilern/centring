
pub struct VM;

pub struct VMProcess;

pub enum VMError {
    ArgcMismatch(usize, usize)
}
