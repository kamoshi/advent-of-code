#![allow(dead_code)]
use std::ops::{Index, IndexMut};
use std::slice::{ChunksExact, Iter};


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

impl<T> Matrix<T> {
    #[inline(always)]
    pub fn shape(&self) -> (usize, usize) {
        (self.rows, self.cols)
    }

    pub fn reshape(mut self, (rows, cols): (usize, usize)) -> Self {
        assert_eq!(self.rows * self.cols, rows * cols);
        (self.rows, self.cols) = (rows, cols);
        self
    }

    pub fn reshape_rows(self, rows: usize) -> Self {
        let cols = self.cols / rows;
        self.reshape((rows, cols))
    }

    pub fn reshape_cols(self, cols: usize) -> Self {
        let rows = self.rows / cols;
        self.reshape((rows, cols))
    }

    pub fn iter_rows(&self) -> ChunksExact<'_, T> {
        self.array.chunks_exact(self.cols)
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.array.iter()
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

    pub fn cell_indices(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        (0..self.rows).into_iter()
            .flat_map(|row| (0..self.cols).into_iter().map(move |col| (row, col)))
    }

    #[inline(always)]
    fn get_offset(&self, row: usize, col: usize) -> usize {
        row * self.cols + col
    }
}

impl<T> Index<(usize, usize)> for Matrix<T> {
    type Output = T;

    fn index(&self, (row, col): (usize, usize)) -> &Self::Output {
        let index = self.get_offset(row, col);
        &self.array[index]
    }
}

impl<T> IndexMut<(usize, usize)> for Matrix<T> {
    fn index_mut(&mut self, (row, col): (usize, usize)) -> &mut Self::Output {
        let index = self.get_offset(row, col);
        &mut self.array[index]
    }
}

pub struct MatrixIter<T> {
    matrix: Matrix<T>,
    index: usize,
}

impl<T: Copy> IntoIterator for Matrix<T> {
    type Item = T;
    type IntoIter = MatrixIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        MatrixIter { matrix: self, index: 0 }
    }
}

impl<T: Copy> Iterator for MatrixIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self.index < self.matrix.array.len() {
            true => {
                let current = self.index;
                self.index += 1;
                Some(self.matrix.array[current])
            }
            false => None
        }
    }
}

impl<T> FromIterator<T> for Matrix<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
        let array = iter.into_iter().collect::<Vec<_>>();
        let cols = array.len();
        Matrix { array, cols, rows: 1 }
    }
}
