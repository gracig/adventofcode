use std::{error::Error, path::Path};

use crate::part2::parser::Model;

pub fn process(filename: &str) -> Result<(), Box<dyn Error>> {
    let model = Model::try_from(Path::new(filename))?;
    println!("Answer is {}", model.total_winings());
    Ok(())
}

mod parser {
    use std::{
        collections::{BTreeSet, HashMap},
        fs::{self},
        path::Path,
    };

    use nom::{
        character::complete::{alphanumeric1, digit1, multispace1, space1},
        combinator::{map, map_res},
        multi::separated_list1,
        sequence::separated_pair,
        IResult,
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    enum Card {
        J,
        Two,
        Three,
        Four,
        Five,
        Six,
        Seven,
        Eight,
        Nine,
        T,
        Q,
        K,
        A,
    }
    impl TryFrom<char> for Card {
        type Error = String;
        fn try_from(value: char) -> Result<Self, Self::Error> {
            let card = match value {
                '2' => Card::Two,
                '3' => Card::Three,
                '4' => Card::Four,
                '5' => Card::Five,
                '6' => Card::Six,
                '7' => Card::Seven,
                '8' => Card::Eight,
                '9' => Card::Nine,
                'T' => Card::T,
                'J' => Card::J,
                'Q' => Card::Q,
                'K' => Card::K,
                'A' => Card::A,
                _ => Err("Not recognized card")?,
            };
            Ok(card)
        }
    }
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum HandType {
        HighCard,
        OnePair,
        TwoPair,
        ThreeOfAKind,
        FullHouse,
        FourOfAKind,
        FiveOfAKind,
    }

    impl From<&[Card; 5]> for HandType {
        fn from(value: &[Card; 5]) -> Self {
            let (kind, jokers) = value
                .iter()
                .fold(HashMap::new(), |mut acc: HashMap<Card, u8>, card| {
                    if let Some(count) = acc.get_mut(card) {
                        *count += 1;
                    } else {
                        acc.insert(*card, 1);
                    }
                    acc
                })
                .into_iter()
                .enumerate()
                .fold(
                    (HandType::HighCard, 0),
                    |(acc, mut jokers), (index, (card, total))| {
                        if let Card::J = card {
                            (acc, jokers + total)
                        } else {
                            let kind = if index == 0 {
                                if total == 5 {
                                    HandType::FiveOfAKind
                                } else if total == 4 {
                                    HandType::FourOfAKind
                                } else if total == 3 {
                                    HandType::ThreeOfAKind
                                } else if total == 2 {
                                    HandType::OnePair
                                } else {
                                    HandType::HighCard
                                }
                            } else {
                                match acc {
                                    HandType::HighCard => {
                                        if total == 4 {
                                            HandType::FourOfAKind
                                        } else if total == 3 {
                                            HandType::ThreeOfAKind
                                        } else if total == 2 {
                                            HandType::OnePair
                                        } else {
                                            HandType::HighCard
                                        }
                                    }
                                    HandType::OnePair => {
                                        if total == 3 {
                                            HandType::FullHouse
                                        } else if total == 2 {
                                            HandType::TwoPair
                                        } else {
                                            HandType::OnePair
                                        }
                                    }
                                    HandType::TwoPair => HandType::TwoPair,
                                    HandType::ThreeOfAKind => {
                                        if total == 2 {
                                            HandType::FullHouse
                                        } else {
                                            HandType::ThreeOfAKind
                                        }
                                    }
                                    HandType::FullHouse => HandType::FullHouse,
                                    HandType::FourOfAKind => HandType::FourOfAKind,
                                    HandType::FiveOfAKind => HandType::FiveOfAKind,
                                }
                            };
                            (kind, jokers)
                        }
                    },
                );
            if jokers == 0 {
                kind
            } else {
                match kind {
                    HandType::FiveOfAKind => HandType::FiveOfAKind,
                    HandType::FullHouse => HandType::FullHouse,
                    HandType::TwoPair => HandType::FullHouse,
                    HandType::FourOfAKind => HandType::FiveOfAKind,
                    HandType::ThreeOfAKind => {
                        if jokers == 2 {
                            HandType::FiveOfAKind
                        } else {
                            HandType::FourOfAKind
                        }
                    }
                    HandType::OnePair => {
                        if jokers == 3 {
                            HandType::FiveOfAKind
                        } else if jokers == 2 {
                            HandType::FourOfAKind
                        } else {
                            HandType::ThreeOfAKind
                        }
                    }
                    HandType::HighCard => {
                        if jokers == 4 || jokers == 5{
                            HandType::FiveOfAKind
                        } else if jokers == 3 {
                            HandType::FourOfAKind
                        } else if jokers == 2 {
                            HandType::ThreeOfAKind
                        } else {
                            HandType::OnePair
                        }
                    }
                }
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Hand {
        kind: HandType,
        cards: [Card; 5],
        bid: u64,
    }

    #[derive(Debug)]
    pub struct Model {
        hands: BTreeSet<Hand>,
    }

    impl Model {
        pub fn total_winings(&self) -> u64 {
            self.hands
                .iter()
                .enumerate()
                .filter(|h| {
                    println!("{:?})", h);
                    true
                })
                .fold(0, |acc, (rank, hand)| acc + hand.bid * (rank as u64 + 1))
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
        map(separated_list1(multispace1, hand), |hands| Model {
            hands: BTreeSet::from_iter(hands),
        })(input)
    }

    fn hand(input: &str) -> IResult<&str, Hand> {
        map_res(
            separated_pair(alphanumeric1, space1, number),
            |(cards, bid)| {
                let cards: [Card; 5] = cards
                    .chars()
                    .map(Card::try_from)
                    .collect::<Result<Vec<Card>, _>>()?
                    .try_into()
                    .expect("Could not tranform cards");
                Ok::<Hand, String>(Hand {
                    kind: HandType::from(&cards),
                    cards,
                    bid,
                })
            },
        )(input)
    }
    fn number(input: &str) -> IResult<&str, u64> {
        map_res(digit1, str::parse::<u64>)(input)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_rhand() {
            let hand = hand("32T3K 765\nT55J5 684").unwrap().1;
            assert_eq!(
                Hand {
                    cards: [Card::Three, Card::Two, Card::T, Card::Three, Card::K],
                    kind: HandType::OnePair,
                    bid: 765
                },
                hand
            );
        }

        #[test]
        fn test_records() {
            let model = model("32T3K 765\nT55J5 684").unwrap().1;
            let rs: [Hand; 2] = model
                .hands
                .into_iter()
                .collect::<Vec<Hand>>()
                .try_into()
                .unwrap_or_else(|v: Vec<Hand>| panic!("Expected {}", v.len()));
            assert_eq!(
                Hand {
                    cards: [Card::Three, Card::Two, Card::T, Card::Three, Card::K],
                    kind: HandType::OnePair,
                    bid: 765
                },
                rs[0]
            );
            assert_eq!(
                Hand {
                    cards: [Card::T, Card::Five, Card::Five, Card::J, Card::Five],
                    kind: HandType::ThreeOfAKind,
                    bid: 684
                },
                rs[1]
            );
        }
    }
}
