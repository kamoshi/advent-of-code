#![allow(dead_code)]
use std::ops::{Deref, Index, IndexMut};


pub struct Matrix<T> {
    array: Vec<T>,
    rows: usize,
    cols: usize,
}

impl<T: Default + Clone> Matrix<T> {
    pub fn new(rows: usize, cols: usize) -> Self {
        Self { rows, cols, array: vec![Default::default(); rows * cols] }
    }
}

impl<T> Matrix<T> {
    pub fn reshape(mut self, rows: usize, cols: usize) -> Matrix<T> {
        assert_eq!(self.rows * self.cols, rows * cols);
        self.rows = rows;
        self.cols = cols;
        self
    }

    #[inline(always)]
    fn get_at(&self, row: usize, col: usize) -> &T {
        let offset = self.get_offset(row, col);
        &self.array[offset]
    }

    #[inline(always)]
    fn get_mut_at(&mut self, row: usize, col: usize) -> &mut T {
        let offset = self.get_offset(row, col);
        &mut self.array[offset]
    }

    #[inline(always)]
    fn set_at(&mut self, row: usize, col: usize, value: T) {
        let offset = self.get_offset(row, col);
        self.array[offset] = value;
    }

    #[inline(always)]
    fn get_offset(&self, row: usize, col: usize) -> usize {
        row * self.cols + col
    }
}

impl<T> Index<usize> for Matrix<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, index: usize) -> &Self::Output {
        &self.array[index]
    }
}

impl<T> IndexMut<usize> for Matrix<T> {
    #[inline(always)]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.array[index]
    }
}

impl<T> Deref for Matrix<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.array
    }
}
