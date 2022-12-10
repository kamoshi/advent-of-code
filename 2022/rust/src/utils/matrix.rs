#![allow(dead_code)]
use std::ops::{Deref, Index, IndexMut};
use std::slice::ChunksExact;


pub struct Matrix<T> {
    array: Vec<T>,
    rows: usize,
    cols: usize,
}

impl<T: Default + Clone> Matrix<T> {
    pub fn new(rows: usize, cols: usize) -> Self {
        Self { rows, cols, array: vec![Default::default(); rows * cols] }
    }

    pub fn with_shape((rows, cols): (usize, usize), value: T) -> Self {
        Self { rows, cols, array: vec![value.clone(); rows * cols] }
    }
}

// impl<'a, T: Copy + 'a> Matrix<T> {
//     pub fn build_from<N, O>(container: O) -> Self
//     where
//         N: IntoIterator<Item=&'a T> + 'a,
//         O: IntoIterator<Item=&'a N>,
//     {
//         let mut array: Vec<T> = Vec::new();
//         let mut cols = Vec::new();
//         for row in container {
//             let len = (&row).into_iter()
//                 .fold(0, |cols, item| {
//                     array.push(*item);
//                     cols + 1
//                 });
//             cols.push(len);
//         };
//         Self { array, rows: array.len() / cols[0], cols: cols[0] }
//     }
// }

impl<T> Matrix<T> {
    pub fn shape(&self) -> (usize, usize) {
        (self.rows, self.cols)
    }

    pub fn iter_rows(&self) -> ChunksExact<'_, T> {
        self.array.chunks_exact(self.cols)
    }

    pub fn reshape(mut self, rows: usize, cols: usize) -> Matrix<T> {
        assert_eq!(self.rows * self.cols, rows * cols);
        self.rows = rows;
        self.cols = cols;
        self
    }

    pub fn get_at(&self, row: usize, col: usize) -> &T {
        let offset = self.get_offset(row, col);
        &self.array[offset]
    }

    pub fn get_mut_at(&mut self, row: usize, col: usize) -> &mut T {
        let offset = self.get_offset(row, col);
        &mut self.array[offset]
    }

    pub fn set_at(&mut self, row: usize, col: usize, value: T) {
        let offset = self.get_offset(row, col);
        self.array[offset] = value;
    }

    fn get_offset(&self, row: usize, col: usize) -> usize {
        row * self.cols + col
    }
}

impl<T> Index<usize> for Matrix<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.array[index]
    }
}

impl<T> IndexMut<usize> for Matrix<T> {
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
