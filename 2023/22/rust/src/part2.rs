use std::{error::Error, path::Path};

use crate::part2::data::Model;

pub fn process(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = Model::try_from(Path::new(filename))?;
    println!("Answer for {} is {}", filename, model.answer());
    Ok(())
}

mod data {
    #[derive(Debug)]
    pub struct Model {}

    impl Model {
        pub fn answer(&self) -> &'static str {
            "Dummy"
        }
    }

    mod parser {
        use nom::{
            character::complete::{digit1, multispace1, space1},
            combinator::{map, map_res},
            multi::separated_list1,
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
            map(separated_list1(multispace1, separated_list1(space1, number)), |_a| Model {})(input)
        }

        fn number(input: &str) -> IResult<&str, u64> {
            map_res(digit1, str::parse::<u64>)(input)
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
