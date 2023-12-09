use std::{error::Error, path::Path};

use crate::part2::data::Model;

pub fn process(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = Model::try_from(Path::new(filename))?;
    println!("Answer for {} is {}", filename, model.answer());
    Ok(())
}

mod data {
    #[derive(Debug)]
    pub struct Model {
        numbers: Vec<Vec<i64>>,
    }

    impl Model {
        pub fn answer(&self) -> i64 {
            self.numbers
                .iter()
                .fold(0, |acc, sequence| acc + self.extrapolate(sequence))
        }

        pub fn differences(&self, ns: &[i64]) -> Option<Vec<i64>> {
            if ns.len() < 2 || ns.iter().all(|v| *v == 0) {
                None
            } else {
                Some(
                    ns.iter()
                        .fold((vec![], None), |(mut acc, before), a| match before {
                            Some(b) => {
                                acc.push(a - b);
                                (acc, Some(a))
                            }
                            None => (acc, Some(a)),
                        })
                        .0,
                )
            }
        }

        pub fn build_stack(&self, ns: &[i64]) -> Vec<Vec<i64>> {
            let mut stack: Vec<Vec<i64>> = vec![ns.to_vec()];
            while let Some(d) = self.differences(stack.last().unwrap()) {
                stack.push(d);
            }
            stack
        }

        pub fn extrapolate(&self, ns: &[i64]) -> i64 {
            let mut stack = self.build_stack(ns);
            if stack.pop().is_some() {
                let mut acc = 0;
                while let Some(mut b) = stack.pop() {
                    b.reverse();
                    acc = b.last().unwrap() - acc
                }
                acc
            } else {
                0
            }
        }
    }

    mod parser {
        use nom::{
            character::complete::{char, digit1, multispace1, space1},
            combinator::{map, map_res, opt, recognize},
            multi::separated_list1,
            sequence::pair,
            IResult,
        };
        use std::{
            fs::{self},
            path::Path,
        };

        use super::Model;

        impl TryFrom<&Path> for Model {
            type Error = String;
            fn try_from(value: &Path) -> Result<Self, Self::Error> {
                fs::read_to_string(value)
                    .map_err(|e| e.to_string())
                    .and_then(|contents| Model::try_from(contents.as_str()))
            }
        }

        impl TryFrom<&str> for Model {
            type Error = String;
            fn try_from(value: &str) -> Result<Self, Self::Error> {
                model(value).map(|(_, m)| m).map_err(|e| e.to_string())
            }
        }

        fn model(input: &str) -> IResult<&str, Model> {
            map(separated_list1(multispace1, numbers), |numbers| Model {
                numbers,
            })(input)
        }

        fn numbers(input: &str) -> IResult<&str, Vec<i64>> {
            separated_list1(space1, number)(input)
        }

        fn number(input: &str) -> IResult<&str, i64> {
            map_res(recognize(pair(opt(char('-')), digit1)), str::parse::<i64>)(input)
        }

        #[cfg(test)]
        mod test {
            use super::*;

            #[test]
            fn test_number() {
                assert_eq!(number("50"), Ok(("", 50)));
            }
        }
    }
}
