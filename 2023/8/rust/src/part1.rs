use std::{error::Error, path::Path};

use crate::part1::parser::Model;

pub fn process(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = Model::try_from(Path::new(filename))?;
    println!("Answer = {}", model.how_many_steps("AAA"));
    Ok(())
}

mod parser {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{alphanumeric1, char, multispace1},
        combinator::{map, map_res},
        multi::{many1, separated_list1},
        sequence::{pair, preceded, separated_pair, tuple},
        IResult,
    };
    use std::{
        collections::HashMap,
        fs::{self},
        path::Path
    };

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    enum Direction {
        Right,
        Left,
    }

    impl TryFrom<char> for Direction {
        type Error = String;
        fn try_from(value: char) -> Result<Self, Self::Error> {
            if value == 'L' {
                Ok(Direction::Left)
            } else if value == 'R' {
                Ok(Direction::Right)
            } else {
                Err("Char does not exist".to_owned())
            }
        }
    }

    #[derive(Debug)]
    pub struct Model {
        directions: Vec<Direction>,
        tree: HashMap<String, (String, String)>,
    }

    impl Model {
        pub fn how_many_steps(&self, initial: &str) -> u64 {
            let mut initial = initial;
            let mut counter = 0;
            for d in self.directions.iter().cycle() {
                if initial == "ZZZ" {
                    break;
                }
                counter += 1;
                let (left, right) = self.tree.get(initial).unwrap();
                match d {
                    Direction::Right => initial = right,
                    Direction::Left => initial = left,
                }
            }
            counter
        }
    }

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
        map(
            separated_pair(
                many1(direction),
                multispace1,
                separated_list1(pair(tag(")"), multispace1), node),
            ),
            |(directions, nodes)| Model {
                directions,
                tree: nodes
                    .into_iter()
                    .fold(HashMap::new(), |mut acc, (label, left, right)| {
                        acc.insert(label.to_string(), (left.to_string(), right.to_string()));
                        acc
                    }),
            },
        )(input)
    }

    fn direction(input: &str) -> IResult<&str, Direction> {
        map_res(alt((char('L'), char('R'))), Direction::try_from)(input)
    }

    fn node(input: &str) -> IResult<&str, (&str, &str, &str)> {
        tuple((
            alphanumeric1,
            preceded(tag(" = ("), alphanumeric1),
            preceded(tag(", "), alphanumeric1),
        ))(input)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_node() {
            assert_eq!(node("AAA = (BBB, CCC)"), Ok((")", ("AAA", "BBB", "CCC"))));
        }
        #[test]
        fn test_direction() {
            assert_eq!(direction("LR"), Ok(("R", Direction::Left)));
            assert_eq!(direction("RL"), Ok(("L", Direction::Right)));
        }

        #[test]
        fn test_model() {
            let model = Model::try_from("RL\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)").unwrap();
            println!("{:?}", model);
        }
    }
}
