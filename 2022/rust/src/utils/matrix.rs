#![allow(dead_code)]
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::{ChunksExact, Iter};


pub struct Matrix<T> {
    array: Vec<T>,
    rows: usize,
    cols: usize,
    offset_r: usize,
    offset_c: usize,
}

impl<T: Default + Clone> Matrix<T> {
    pub fn new(rows: usize, cols: usize) -> Self {
        Self { rows, cols, array: vec![Default::default(); rows * cols], offset_r: 0, offset_c: 0 }
    }
}

impl<T: Clone> Matrix<T> {
    pub fn with_shape((rows, cols): (usize, usize), value: T) -> Self {
        Self { rows, cols, array: vec![value.clone(); rows * cols], offset_r: 0, offset_c: 0 }
    }

    pub fn with_bounds((min_r, min_c): (usize, usize), (max_r, max_c): (usize, usize), value: T) -> Self {
        assert!(max_r > min_r && max_c > min_c, "Min bound has to be lower than max bound");
        let (rows, cols) = (max_r - min_r + 1, max_c - min_c + 1);
        let (offset_r, offset_c) = (min_r, min_c);
        Self { rows, cols, array: vec![value.clone(); rows * cols], offset_r, offset_c }
    }

    pub fn draw_line(&mut self, (r1, c1): (usize, usize), (r2, c2): (usize, usize), value: T) {
        let (min_r, max_r) = (r1.min(r2), r1.max(r2));
        let (min_c, max_c) = (c1.min(c2), c1.max(c2));
        match (r1 == r2, c1 == c2) {
            (true, true) => self[(r1, c1)] = value.clone(),
            (true, _) => (min_c..=max_c).into_iter()
                .for_each(|c| self[(r1, c)] = value.clone()),
            (_, true) => (min_r..=max_r).into_iter()
                .for_each(|r| self[(r, c1)] = value.clone()),
            _ => unimplemented!(),
        };
    }
}

impl<T> Matrix<T> {
    #[inline(always)]
    pub fn get_shape(&self) -> (usize, usize) {
        (self.rows, self.cols)
    }

    pub fn get_bounds(&self) -> ((usize, usize), (usize, usize)) {
        ((self.offset_r, self.offset_c), (self.offset_r + self.rows, self.offset_c + self.cols))
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

    pub fn cell_indices(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        let row_range = self.offset_r..(self.rows + self.offset_r);
        let col_range = self.offset_c..(self.cols + self.offset_c);
        row_range.into_iter()
            .flat_map(move |row| col_range.clone().into_iter().map(move |col| (row, col)))
    }

    #[inline(always)]
    fn get_offset(&self, row: usize, col: usize) -> usize {
        (row - self.offset_r) * self.cols + (col - self.offset_c)
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
        Matrix { array, cols, rows: 1, offset_r: 0, offset_c: 0  }
    }
}

impl<T> fmt::Display for Matrix<T> where T: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in self.iter_rows() {
            for item in row {
                write!(f, "{}", item)?
            };
            write!(f, "\n")?;
        };
        Ok(())
    }
}

