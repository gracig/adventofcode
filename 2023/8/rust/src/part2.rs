use std::{error::Error, path::Path};

use crate::part2::parser::Model;

pub fn process(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = Model::try_from(Path::new(filename))?;
    println!("Answer = {}", model.how_many_steps());
    Ok(())
}

mod parser {
    use core::time;
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
        path::Path,
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
        pub fn how_many_steps(&self) -> u128 {
            let mut next = self
                .tree
                .keys()
                .filter(|k| k.ends_with('A'))
                .map(|s| (s, s, 0, 0))
                .collect::<Vec<(&String, &String, u128, u128)>>();
            let mut counter = 0;
            let mut finished: Vec<(u128, u128)> = vec![];
            println!("Initial {:?}", next);
            for d in self.directions.iter().cycle() {
                let mut remaining: Vec<(&String, &String, u128, u128)> = vec![];
                for (start, node, mut c1, mut c2) in next.into_iter() {
                    let (left, right) = self.tree.get(node).unwrap();
                    println!(
                        "counter {}  anode {} znode: {}  beforez: {} : afterz: {}",
                        counter, start, node, c1, c2
                    );
                    if node.ends_with('Z') {
                        if c1 == 0 {
                            println!("Setting beforez to {}", counter);
                            c1 = counter;
                        } else if c2 == 0 {
                            println!("Setting afterz to {}", counter);
                            c2 = counter;
                            finished.push((c1, c2 - c1));
                            continue;
                        }
                    }
                    match d {
                        Direction::Right => remaining.push((start, right, c1, c2)),
                        Direction::Left => remaining.push((start, left, c1, c2)),
                    };
                }
                if remaining.is_empty() {
                    println!("Remaining is empty");
                    break;
                }
                next = remaining;
                counter += 1;
            }
            lcm_of_znodes(&finished)
            //counter
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

    fn gcd(a: u128, b: u128) -> u128 {
        if b == 0 {
            a
        } else {
            gcd(b, a % b)
        }
    }

    fn lcm(a: u128, b: u128) -> u128 {
        a / gcd(a, b) * b
    }

    fn first_multiple_after(beforez: u128, afterz: u128) -> u128 {
        let remainder = beforez % afterz;
        if remainder == 0 {
            beforez
        } else {
            beforez + afterz - remainder
        }
    }

    fn lcm_of_znodes(znodes: &[(u128, u128)]) -> u128 {
        znodes
            .iter()
            .map(|&(beforez, afterz)| {
                let faz =  first_multiple_after(beforez, afterz);
                println!("First multipler after z: {}", faz);

                faz
                })
            .fold(1, |acc, x| {
                let l = lcm(acc, x);
                println!("LCM of {} and {} is {}", acc, x, l);
                l
            })
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
