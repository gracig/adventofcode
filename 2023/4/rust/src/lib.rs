use std::{
    collections::HashMap,
    error::Error,
    fs::File,
    io::{self, BufRead},
    path::Path
};

use parser::Game;

pub fn process_part_1(filename: &str) -> Result<(), Box<dyn Error>> {
    let answer = read_lines(filename)?
        .map_while(Result::ok)
        .filter_map(|l| Game::try_from(l.as_str()).ok())
        .fold(0, |acc, game| {
            println!("{:?} {}", game, game.points());
            acc + game.points()
        });

    println!("Answer is: {}", answer);
    Ok(())
}

pub fn process_part_2(filename: &str) -> Result<(), Box<dyn Error>> {
    let answer = read_lines(filename)?
        .map_while(Result::ok)
        .filter_map(|l| Game::try_from(l.as_str()).ok())
        .fold(HashMap::new(), |mut acc: HashMap<u32, u32>, game| {
            let add_counter = |map: &mut HashMap<u32, u32>, id: u32, count: u32| {
                if let Some(total) = map.get_mut(&id) {
                    *total += count;
                    *total
                } else {
                    map.insert(id, count);
                    count
                }
            };
            let copies = add_counter(&mut acc, game.id, 1);
            let mut i = 0;
            let matching = game.matching_numbers();
            while i < matching {
                let next_id = game.id + i + 1;
                add_counter(&mut acc,next_id ,copies);
                i+=1;
            }
            println!("Card {:3} | Matching {:2} | Copies {}", game.id, matching, copies);
            acc
        })
        .into_iter()
        .fold(0, |acc, e| {
            //println!("{:?}", e);
            acc + e.1
        });

    println!("Answer is: {}", answer);
    Ok(())
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

mod parser {
    use nom::bytes::complete::tag;
    use nom::character::complete::{digit1, space0, space1};
    use nom::combinator::{map, map_res};
    use nom::multi::separated_list1;
    use nom::sequence::{pair, preceded, separated_pair, tuple};
    use nom::IResult;
    use std::collections::HashSet;

    #[derive(Debug)]
    pub struct Game {
        pub id: u32,
        pub winning: HashSet<u32>,
        pub player: HashSet<u32>,
    }

    impl TryFrom<&str> for Game {
        type Error = String;
        fn try_from(value: &str) -> Result<Self, Self::Error> {
            game(value).map(|(_, g)| g).map_err(|e| e.to_string())
        }
    }

    impl Game {
        pub fn points(&self) -> u32 {
            self.player.iter().fold(0, |acc, pn| {
                if self.winning.contains(pn) {
                    if acc == 0 {
                        1
                    } else {
                        acc * 2
                    }
                } else {
                    acc
                }
            })
        }
        pub fn matching_numbers(&self) -> u32 {
            self.player.iter().fold(0, |acc, pn| {
                if self.winning.contains(pn) {
                    acc + 1
                } else {
                    acc
                }
            })
        }
    }

    fn game(input: &str) -> IResult<&str, Game> {
        map(
            separated_pair(game_id, tuple((space0, tag(":"), space0)), cards),
            |(id, (winning, player))| Game {
                id,
                winning,
                player,
            },
        )(input)
    }
    fn game_id(input: &str) -> IResult<&str, u32> {
        preceded(pair(tag("Card"), space1), number)(input)
    }

    fn cards(input: &str) -> IResult<&str, (HashSet<u32>, HashSet<u32>)> {
        separated_pair(numbers, tuple((space0, tag("|"), space0)), numbers)(input)
    }

    fn numbers(input: &str) -> IResult<&str, HashSet<u32>> {
        map(
            separated_list1(space1, preceded(space0, number)),
            HashSet::from_iter,
        )(input)
    }

    fn number(input: &str) -> IResult<&str, u32> {
        map_res(digit1, |s: &str| s.parse::<u32>())(input)
    }

    #[cfg(test)]
    mod test {
        use super::*;
        #[test]
        fn test_cube() {
            let numbers = numbers("    10 20 30    |   ").unwrap().1;
            assert!(numbers.contains(&10));
            assert!(numbers.contains(&20));
            assert!(numbers.contains(&30));
        }
        #[test]
        fn test_cards() {
            let (w, p) = cards("      97 80 |   85 65 80    ").unwrap().1;

            assert!(w.contains(&97));
            assert!(w.contains(&80));
            assert!(p.contains(&85));
            assert!(p.contains(&65));
            assert!(p.contains(&80));
        }

        #[test]
        fn test_game_id() {
            assert_eq!(game_id("Card 186:    97"), Ok((":    97", 186)));
        }

        #[test]
        fn test_game() {
            let game = game("Card       186:      97  80    |    85    65   80   ")
                .unwrap()
                .1;
            assert_eq!(game.id, 186);
            assert!(game.winning.contains(&97));
            assert!(game.winning.contains(&80));
            assert!(game.player.contains(&85));
            assert!(game.player.contains(&65));
            assert!(game.player.contains(&80));
        }
    }
}
