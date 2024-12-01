use std::{fmt::Debug, time::Instant};

pub type Error = Box<dyn std::error::Error>;

pub trait Solution: Debug {}

struct SolutionExists<A, B>(A, B);

impl<A, B> Debug for SolutionExists<A, B>
where
    A: Debug,
    B: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "A: {:?}", self.0)?;
        writeln!(f, "B: {:?}", self.1)?;
        Ok(())
    }
}

impl<A, B> Solution for SolutionExists<A, B>
where
    A: Debug,
    B: Debug,
{
}

type Parser<A> = fn(&str) -> Result<A, Error>;
type Solver<A, B> = fn(&A) -> B;
type Runner = Box<dyn Fn(&str) -> Result<Box<dyn Solution + 'static>, Error>>;
pub type Day = (usize, Runner);

pub fn mk_day<A, B, C>(day: usize, parse: Parser<A>, a: Solver<A, B>, b: Solver<A, C>) -> Day
where
    A: 'static,
    B: Debug + 'static,
    C: Debug + 'static,
{
    (
        day,
        Box::new(move |input| {
            let text = parse(input)?;

            let start = Instant::now();
            let a = a(&text);
            println!("Execution time A: {:?}", start.elapsed());

            let start = Instant::now();
            let b = b(&text);
            println!("Execution time B: {:?}", start.elapsed());

            Ok(Box::new(SolutionExists(a, b)))
        }),
    )
}

#[doc(inline)]
pub use crate::__day as day;

#[allow(clippy::crate_in_macro_def)]
#[doc(hidden)]
#[macro_export]
macro_rules! __day {
    ($day_num:expr, $parse:ident, $solve_a:ident, $solve_b:ident) => {
        pub fn day() -> crate::advent::Day {
            crate::advent::mk_day($day_num, $parse, $solve_a, $solve_b)
        }
    };
}
