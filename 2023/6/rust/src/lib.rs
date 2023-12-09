use std::{error::Error, path::Path};

use crate::parser::Model;

pub fn process_part_1(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = Model::try_from(Path::new(filename))?;
    println!("Answer = {}", model.find_ways_to_beat2());
    Ok(())
}

pub fn process_part_2(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = Model::try_from(Path::new(filename))?;
    let model = model.derive();
    println!("New model {:?}", model);
    println!("Answer = {}", model.find_ways_to_beat2());
    Ok(())
}

mod parser {
    use std::{
        fs::{self},
        path::Path,
    };

    use nom::{
        bytes::complete::tag,
        character::complete::{digit1, multispace1, space1},
        combinator::{map, map_res},
        multi::separated_list1,
        sequence::{pair, preceded, separated_pair},
        IResult,
    };
    #[derive(Debug)]
    pub struct Model {
        records: Vec<Record>,
    }
    impl Model {
        pub fn find_ways_to_beat2(&self) -> u64 {
            self.records
                .iter()
                .map(|r| {
                    let number = r.find_ways_to_beat2();
                    println!("result for Record {:?} is {}", r, number);
                    number
                })
                .product()
        }
        pub fn derive(&self) -> Self {
            let t = self.records.iter().fold(
                ("".to_string(), "".to_string()),
                |(mut time, mut record), r| {
                    time.push_str(r.time.to_string().as_str());
                    record.push_str(r.record.to_string().as_str());
                    (time, record)
                },
            );

            Model {
                records: vec![Record {
                    time: t.0.parse::<u64>().unwrap(),
                    record: t.1.parse::<u64>().unwrap(),
                }],
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    struct Record {
        time: u64,
        record: u64,
    }
    impl Record {
        fn find_ways_to_beat2(&self) -> u64 {
            if self.time < 2 {
                0
            } else {
                self.right(self.time / 2, self.time) - self.left(0, self.time / 2)
            }
        }
        fn left(&self, start: u64, finish: u64) -> u64 {
            let mut l = start;
            let mut r = finish;
            let mut distance = 0;
            while l < r {
                let pressed = (l + r) / 2;
                let remaining_time = self.time - pressed;
                distance = pressed * remaining_time;
                println!(
                    "Comparing pressed:{} distance:{} with :{}",
                    pressed, distance, self.record
                );
                if distance > self.record - 1 {
                    r = pressed
                } else {
                    l = pressed + 1
                }
            }
            println!("leftmost: {} {}", r, distance);
            r
        }
        fn right(&self, start: u64, finish: u64) -> u64 {
            let mut l = start;
            let mut r = finish;
            let mut distance = 0;
            while l < r {
                let pressed = (l + r) / 2;
                let remaining_time = self.time - pressed;
                distance = pressed * remaining_time;
                println!(
                    "Comparing pressed:{} distance:{} with :{}",
                    pressed, distance, self.record
                );
                if distance > self.record {
                    l = pressed + 1
                } else {
                    r = pressed
                }
            }
            println!("rightmost: {}  {}", r, distance);
            r
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
        map(records, |records| Model { records })(input)
    }

    fn records(input: &str) -> IResult<&str, Vec<Record>> {
        map(separated_pair(time, multispace1, distance), |(t, d)| {
            t.into_iter()
                .zip(d)
                .map(|(time, record)| Record { time, record })
                .collect()
        })(input)
    }
    fn time(input: &str) -> IResult<&str, Vec<u64>> {
        preceded(pair(tag("Time:"), space1), separated_list1(space1, number))(input)
    }
    fn distance(input: &str) -> IResult<&str, Vec<u64>> {
        preceded(
            pair(tag("Distance:"), space1),
            separated_list1(space1, number),
        )(input)
    }
    fn number(input: &str) -> IResult<&str, u64> {
        map_res(digit1, str::parse::<u64>)(input)
    }

    #[cfg(test)]
    mod test {
        use super::*;
        #[test]
        fn test_records() {
            let rs = records("Time:      7  15   30\nDistance:  9  40  200")
                .unwrap()
                .1;
            let rsarray: [Record; 3] = rs.try_into().unwrap_or_else(|v: Vec<Record>| {
                panic!("Expected a Vec of length {} but it was {}", 3, v.len())
            });
            assert_eq!(Record { time: 7, record: 9 }, rsarray[0]);
            assert_eq!(
                Record {
                    time: 15,
                    record: 40
                },
                rsarray[1]
            );
            assert_eq!(
                Record {
                    time: 30,
                    record: 200
                },
                rsarray[2]
            );
        }
    }
}
