use std::{error::Error, path::Path};

use crate::parser::Model;

pub fn process_part_1(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = Model::try_from(Path::new(filename))?;
    let answer = model
        .translate_seeds(&[
            "seed-to-soil",
            "soil-to-fertilizer",
            "fertilizer-to-water",
            "water-to-light",
            "light-to-temperature",
            "temperature-to-humidity",
            "humidity-to-location",
        ])
        .into_iter()
        .min()
        .unwrap_or(0);
    println!("The answer is: {}", answer);

    Ok(())
}

pub fn process_part_2(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = Model::try_from(Path::new(filename))?;
    let answer = model.translate_seeds_2(&[
        "seed-to-soil",
        "soil-to-fertilizer",
        "fertilizer-to-water",
        "water-to-light",
        "light-to-temperature",
        "temperature-to-humidity",
        "humidity-to-location",
    ]);
    println!("The answer is: {}", answer);
    Ok(())
}

mod parser {
    use std::{
        collections::HashMap,
        fmt::Display,
        fs::{self},
        path::Path,
    };

    use nom::{
        bytes::complete::{tag, take_until},
        character::{
            complete::{digit1, multispace1, space0},
            streaming::multispace0,
        },
        combinator::{map, map_res},
        multi::separated_list1,
        sequence::{pair, preceded, separated_pair, tuple},
        IResult,
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    struct Interval {
        start: u64,
        end: u64,
        transpose: u64,
    }
    #[derive(Debug, Clone)]
    struct IntervalSet {
        inner: Vec<Interval>,
    }
    impl From<SimpleRange> for IntervalSet {
        fn from(value: SimpleRange) -> Self {
            IntervalSet {
                inner: vec![Interval {
                    start: value.start,
                    end: value.end,
                    transpose: value.start,
                }],
            }
        }
    }
    impl From<RangeMap> for IntervalSet {
        fn from(value: RangeMap) -> Self {
            let mut inner: Vec<Interval> = value
                .entries
                .into_iter()
                .map(|entry| Interval {
                    start: entry.source,
                    end: entry.source + entry.length,
                    transpose: entry.destination,
                })
                .collect();
            inner.sort();
            IntervalSet { inner }
        }
    }

    impl Display for IntervalSet {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.inner.iter().enumerate().try_for_each(|(i, item)| {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}<={}:{}", item.transpose, item.start, item.end)
            })
        }
    }

    impl IntervalSet {
        fn merge(&self, set: &IntervalSet) -> Self {
            let mut result: Vec<Interval> = vec![];
            print!("Merging {} with {} => ", self, set);
            let mut left_iter = self.inner.iter().copied().peekable();
            let mut right_iter = set.inner.iter().copied().peekable();

            while let Some(left_peek) = left_iter.peek_mut() {
                let mut get_next_left = false;
                let mut get_next_right = false;

                match right_iter.peek() {
                    None => {
                        get_next_left = true;
                        result.push(*left_peek)
                    }
                    Some(right_peek) => {
                        if left_peek.end < right_peek.start {
                            get_next_left = true;
                            result.push(*left_peek);
                        } else if right_peek.end < left_peek.start {
                            get_next_right = true;
                        } else if left_peek.start < right_peek.start {
                            result.push(Interval {
                                start: left_peek.start,
                                end: right_peek.start,
                                transpose: left_peek.transpose,
                            });
                            if left_peek.end < right_peek.end {
                                get_next_left = true;
                                result.push(Interval {
                                    start: right_peek.start,
                                    end: left_peek.end,
                                    transpose: right_peek.transpose,
                                })
                            } else {
                                get_next_left = true;
                                get_next_right = true;
                                result.push(Interval {
                                    start: right_peek.start,
                                    end: right_peek.end,
                                    transpose: right_peek.transpose,
                                });
                                result.push(Interval {
                                    start: right_peek.end,
                                    end: left_peek.end,
                                    transpose: left_peek.transpose
                                        + (right_peek.end - left_peek.start),
                                })
                            }
                        } else if left_peek.end < right_peek.end {
                            get_next_left = true;
                            result.push(Interval {
                                start: left_peek.start,
                                end: left_peek.end,
                                transpose: right_peek.transpose
                                    + (left_peek.start - right_peek.start),
                            });
                        } else {
                            get_next_right = true;
                            result.push(Interval {
                                start: left_peek.start,
                                end: right_peek.end,
                                transpose: right_peek.transpose
                                    + (left_peek.start - right_peek.start),
                            });
                            left_peek.start = right_peek.end;
                        }
                    }
                }
                if get_next_left {
                    left_iter.next();
                }
                if get_next_right {
                    right_iter.next();
                }
            }
            let result = IntervalSet::transpose(&IntervalSet { inner: result });
            println!("{}", result);
            result
        }

        fn transpose(set: &IntervalSet) -> Self {
            let mut transposed: Vec<Interval> = set
                .clone()
                .inner
                .into_iter()
                .map(|i| Interval {
                    start: i.transpose,
                    end: i.transpose + (i.end - i.start),
                    transpose: i.transpose,
                })
                .collect();
            transposed.sort();
            IntervalSet { inner: transposed }
        }
    }

    #[derive(Debug, Clone)]
    struct RangeMap {
        name: String,
        entries: Vec<RangeMapEntry>,
    }

    impl RangeMap {
        pub fn translate(&self, value: u64) -> Option<u64> {
            if let Err(Some(v2)) = self.entries.iter().try_fold((), |_, r| {
                if let Some(n) = r.translate(value) {
                    Err(Some(n))
                } else {
                    Ok(())
                }
            }) {
                Some(v2)
            } else {
                None
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct RangeMapEntry {
        pub source: u64,
        pub destination: u64,
        pub length: u64,
    }

    impl RangeMapEntry {
        pub fn translate(&self, n: u64) -> Option<u64> {
            if n >= self.source && n < self.source + self.length {
                Some(self.destination + (n - self.source))
            } else {
                None
            }
        }
    }

    #[derive(Debug)]
    pub struct Model {
        seeds: Vec<u64>,
        maps: HashMap<String, RangeMap>,
    }

    #[derive(Debug, Clone)]
    pub struct SimpleRange {
        start: u64,
        end: u64,
    }

    impl Model {
        pub fn seeds_to_range(&self) -> Vec<SimpleRange> {
            self.seeds
                .iter()
                .fold((vec![], None), |(mut acc, source), value| match source {
                    None => {
                        println!("Source {}", value);
                        (acc, Some(value))
                    }
                    Some(source) => {
                        println!("Length {}", value);
                        acc.push(SimpleRange {
                            start: *source,
                            end: *source + *value,
                        });
                        (acc, None)
                    }
                })
                .0
        }
        pub fn translate_seeds_2(&self, maps: &[&str]) -> u64 {
            self.seeds_to_range()
                .iter()
                .flat_map(|simple_range| {
                    maps.iter()
                        .filter_map(|m| self.maps.get(*m))
                        .fold(IntervalSet::from(simple_range.clone()), |acc, range_map| {
                            acc.merge(&IntervalSet::from(range_map.clone()))
                        })
                        .inner
                        .into_iter()
                })
                .fold(u64::MAX, |min_value, set| {
                    if set.start < min_value {
                        set.start
                    } else {
                        min_value
                    }
                })
        }

        pub fn translate_seeds(&self, maps: &[&str]) -> Vec<u64> {
            self.seeds
                .iter()
                .map(|v| {
                    print!("{}", v);
                    let v2 = maps
                        .iter()
                        .filter_map(|m| self.maps.get(*m))
                        .fold(*v, |acc, m| {
                            let v2 = m.translate(acc).unwrap_or(acc);
                            print!(" -> {}", v2);
                            v2
                        });
                    println!();
                    v2
                })
                .collect()
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
    impl TryFrom<&str> for RangeMapEntry {
        type Error = String;
        fn try_from(value: &str) -> Result<Self, Self::Error> {
            range_map_entry(value)
                .map(|(_, m)| m)
                .map_err(|e| e.to_string())
        }
    }
    impl TryFrom<&str> for RangeMap {
        type Error = String;
        fn try_from(value: &str) -> Result<Self, Self::Error> {
            range_map(value).map(|(_, m)| m).map_err(|e| e.to_string())
        }
    }

    fn model(input: &str) -> IResult<&str, Model> {
        map(
            pair(
                preceded(multispace0, seeds),
                separated_list1(multispace1, range_map),
            ),
            |(seeds, maps)| Model {
                seeds,
                maps: maps.into_iter().fold(HashMap::new(), |mut acc, r| {
                    acc.insert(r.name.to_string(), r);
                    acc
                }),
            },
        )(input)
    }

    fn range_map(input: &str) -> IResult<&str, RangeMap> {
        map(
            separated_pair(
                preceded(multispace0, take_until(" map:")),
                tag(" map:"),
                separated_list1(multispace1, range_map_entry),
            ),
            |(name, entries)| RangeMap {
                name: name.to_owned(),
                entries,
            },
        )(input)
    }

    fn range_map_entry(input: &str) -> IResult<&str, RangeMapEntry> {
        map(
            tuple((
                preceded(multispace0, number),
                preceded(space0, number),
                preceded(space0, number),
            )),
            |(destination, source, length)| RangeMapEntry {
                source,
                destination,
                length,
            },
        )(input)
    }

    fn seeds(input: &str) -> IResult<&str, Vec<u64>> {
        preceded(tag("seeds: "), separated_list1(tag(" "), number))(input)
    }
    fn number(input: &str) -> IResult<&str, u64> {
        map_res(digit1, str::parse::<u64>)(input)
    }

    #[cfg(test)]
    mod test {
        use super::*;
        #[test]
        fn test_range_map_entry() {
            assert_eq!(
                RangeMapEntry::try_from("\n   43 23 2"),
                Ok(RangeMapEntry {
                    source: 23,
                    destination: 43,
                    length: 2
                })
            )
        }
        #[test]
        fn test_range_map() {
            let m =
                RangeMap::try_from("\nsoil-to-seed map:\n2 1 1\n3 2 2\nseedtosoil map:").unwrap();
            assert_eq!(m.name, "soil-to-seed");
            let entries: [RangeMapEntry; 2] =
                m.entries
                    .try_into()
                    .unwrap_or_else(|v: Vec<RangeMapEntry>| {
                        panic!("Expected a Vec of length {} but it was {}", 2, v.len())
                    });
            assert_eq!(
                entries[0],
                RangeMapEntry {
                    source: 1,
                    destination: 2,
                    length: 1
                }
            );
            assert_eq!(
                entries[1],
                RangeMapEntry {
                    source: 2,
                    destination: 3,
                    length: 2
                }
            );
        }

        #[test]
        fn test_model() {
            let m =
                Model::try_from("seeds: 43 40 33 33\nsoil-to-seed map:\n2 1 1\n3 2 2\nseed-to-soil map:\n2 1 1\n3 2 2.").unwrap();
            assert!([43u64, 40, 33, 33].iter().all(|n| m.seeds.contains(n)));
            println!("{:?}", m);
        }
    }
}
