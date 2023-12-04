use std::collections::HashMap;

use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit1, space1, space0, multispace1};
use nom::combinator::{map, map_res};
use nom::multi::separated_list1;
use nom::sequence::{separated_pair, pair, preceded};
use nom::IResult;


#[derive(Debug)]
pub struct Game {
    pub id: u32,
    pub subsets: Subsets
}

pub type Subsets = Vec<Subset>;
pub type Subset = HashMap<Color,u32>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Color {
    RED,
    GREEN,
    BLUE,
}

impl TryFrom<&str> for Game{
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        game(value).map(|(_,g)|g).map_err(|e|e.to_string())
    }
}

impl Game {
    pub fn power(&self) -> u32 {
        self.max_blue() * self.max_green() * self.max_red()
    }
    pub fn max_red(&self) -> u32 {
        self.max_from_color(Color::RED)
    }
    pub fn max_green(&self) -> u32 {
        self.max_from_color(Color::GREEN)
    }
    pub fn max_blue(&self) -> u32 {
        self.max_from_color(Color::BLUE)
    }
    pub fn max_from_color(&self, color: Color) -> u32 {
        self.subsets.iter().fold(0, |acc, seq | 
            if let Some(x) = seq.get(&color) {
                if *x > acc {
                    *x
                }else {
                    acc
                }
            }else{
                acc
            }
        )
    }
}

/*
fn games (input : &str) -> IResult<&str, Vec<Game>> {
    separated_list1(multispace1, game)(input)
}
*/

fn game (input : &str) -> IResult<&str, Game> {
    map(
        separated_pair(game_id, pair(tag(":"), space0),subsets),
        |(id,subsets)| Game{id, subsets} )(input)
}

fn game_id (input : &str) -> IResult<&str, u32 >{
     preceded(tag("Game "), number)(input)
}
fn subsets(input: &str) -> IResult<&str, Subsets> {
    separated_list1(pair(tag(";"),space0),subset)(input)
}
fn subset(input: &str) -> IResult<&str, Subset> {
    map(separated_list1(pair(tag(","),space0), cube),HashMap::from_iter)(input)
}

fn cube(input: &str) -> IResult<&str, (Color, u32)> {
    map(separated_pair(number, space1, color), |(a, b)| (b, a))(input)
}
fn number(input: &str) -> IResult<&str, u32> {
    map_res(digit1, |s: &str| s.parse::<u32>())(input)
}

fn color(input: &str) -> IResult<&str, Color> {
    map_res(alpha1, |s: &str| match s {
        "red" => Ok(Color::RED),
        "blue" => Ok(Color::BLUE),
        "green" => Ok(Color::GREEN),
        _ => Err("color has failed"),
    })(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{
        error::{Error, ErrorKind},
        Err,
    };

    #[test]
    fn test_game() {
        let game = game("Game 1: 4 green, 7 blue; 2 blue, 4 red; 5 blue, 2 green, 2 red; 1 green, 3 red, 9 blue; 3 green, 9 blue; 790 green, 2 blue, 2 red").ok().unwrap().1;
        let subsets: [Subset; 6] = game.subsets.try_into().unwrap_or_else(|v: Subsets| panic!("Expected a Vec of length {} but it was {}", 2, v.len()));
        assert_eq!(subsets[0].get(&Color::GREEN), Some(&4));
        assert_eq!(subsets[0].get(&Color::BLUE), Some(&7));
        assert_eq!(subsets[0].get(&Color::RED), None);
        assert_eq!(subsets[1].get(&Color::GREEN), None);
        assert_eq!(subsets[1].get(&Color::BLUE), Some(&2));
        assert_eq!(subsets[1].get(&Color::RED), Some(&4));
        assert_eq!(subsets[5].get(&Color::GREEN), Some(&790));
        assert_eq!(subsets[5].get(&Color::BLUE), Some(&2));
        assert_eq!(subsets[5].get(&Color::RED), Some(&2));
    }


    #[test]
    fn test_game_id() {
        assert_eq!(game_id("Game 1:"), Ok((":", 1)));
        assert_eq!(game_id("Game 20:"), Ok((":", 20)));
        assert_eq!(game_id("Game 300:"), Ok((":", 300)));
    }

    #[test]
    fn test_parse_quantity() {
        assert_eq!(number("1"), Ok(("", 1)));
        assert_eq!(number("2"), Ok(("", 2)));
        assert_eq!(
            number("apple"),
            Err(Err::Error(Error {
                input: "apple",
                code: ErrorKind::Digit
            }))
        );
    }
    #[test]
    fn test_color() {
        assert_eq!(color("red"), Ok(("", Color::RED)));
        assert_eq!(color("green"), Ok(("", Color::GREEN)));
        assert_eq!(color("blue"), Ok(("", Color::BLUE)));
        assert_eq!(
            color("12"),
            Err(Err::Error(Error {
                input: "12",
                code: ErrorKind::Alpha
            }))
        );
        assert_eq!(
            color("black"),
            Err(Err::Error(Error {
                input: "black",
                code: ErrorKind::MapRes
            }))
        );
    }
    #[test]
    fn test_cube() {
        assert_eq!(cube("10 red\n"), Ok(("\n", (Color::RED, 10))));
        assert_eq!(cube("10 green;"), Ok((";", (Color::GREEN, 10))));
        assert_eq!(cube("10    blue "), Ok((" ", (Color::BLUE, 10))));
    }

    #[test]
    fn test_subset() {
        let map = subset("10 red, 20 green, 30 blue").unwrap().1;
        assert_eq!(map.get(&Color::RED), Some(&10));
        assert_eq!(map.get(&Color::GREEN), Some(&20));
        assert_eq!(map.get(&Color::BLUE), Some(&30))
    }

    #[test]
    fn test_subsets() {
        let xs = subsets("10 red, 20 green, 30 blue; 30 red, 20 green, 10 blue\n").unwrap().1;
        let ys: [HashMap<Color,u32>; 2] = xs.try_into().unwrap_or_else(|v: Vec<HashMap<Color,u32>>| panic!("Expected a Vec of length {} but it was {}", 2, v.len()));
        assert_eq!(ys[0].get(&Color::RED), Some(&10));
        assert_eq!(ys[0].get(&Color::GREEN), Some(&20));
        assert_eq!(ys[0].get(&Color::BLUE), Some(&30));
        assert_eq!(ys[1].get(&Color::RED), Some(&30));
        assert_eq!(ys[1].get(&Color::GREEN), Some(&20));
        assert_eq!(ys[1].get(&Color::BLUE), Some(&10));
    }


}
