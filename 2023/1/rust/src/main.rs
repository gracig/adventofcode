use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};


pub fn main() {
    process("input.txt", find_number_part_1);
    process("input.txt", find_number_part_2);
}


#[derive(Clone, Debug, PartialEq)]
pub enum State {
    Initial,
    O,
    On,
    T,
    Tw,
    Th,
    Thr,
    Thre,
    F,
    Fo,
    Fou,
    Fi,
    Fiv,
    S,
    Si,
    Se,
    Sev,
    Seve,
    E,
    Ei,
    Eig,
    Eigh,
    N,
    Ni,
    Nin,
    Digit,
    Terminate,
}

pub fn get_state(c: char, t: char, move_true: State) -> State {
    if c == t {
        move_true
    } else {
        State::Terminate
    }
}

pub fn get_state_2(c: char, map: HashMap<char, State>) -> State {
    if map.contains_key(&c) {
        let state = map.get(&c).unwrap();
        state.clone()
    } else {
        State::Terminate
    }
}

pub fn get_next_state(c: char) -> State {
    if c.is_ascii_digit() {
        State::Digit
    } else if c == 'o' {
        State::O
    } else if c == 't' {
        State::T
    } else if c == 'f' {
        State::F
    } else if c == 's' {
        State::S
    } else if c == 'e' {
        State::E
    } else if c == 'n' {
        State::N
    } else {
        State::Terminate
    }
}

pub fn update_first_and_last(first: &mut Option<u64>, last: &mut Option<u64>, c: char) -> State {
    if first.is_none() {
        *first = Some(c as u64 - 48)
    }
    *last = Some(c as u64 - 48);
    State::Terminate
}

pub fn find_number_part_2(s: &str) -> Option<u64> {
    let mut first: Option<u64> = None;
    let mut last: Option<u64> = None;
    let mut lastc: Option<char> = None;
    let mut states: Vec<(State, String)> = [].to_vec();
    for c in s.chars() {
        states.push((State::Initial, "".to_owned()));
        for (state, acc) in states.iter_mut() {
            acc.push(c);
            match state {
                State::Initial => *state = get_next_state(c),
                State::Terminate => {}
                State::O => *state = get_state(c, 'n', State::On),
                State::On => {
                    if c == 'e' {
                        update_first_and_last(&mut first, &mut last, '1');
                    }
                    *state = State::Terminate
                }
                State::T => {
                    *state = get_state_2(c, HashMap::from([('w', State::Tw), ('h', State::Th)]))
                }
                State::Tw => {
                    if c == 'o' {
                        update_first_and_last(&mut first, &mut last, '2');
                    }
                    *state = State::Terminate
                }
                State::Th => *state = get_state(c, 'r', State::Thr),
                State::Thr => *state = get_state(c, 'e', State::Thre),
                State::Thre => {
                    if c == 'e' {
                        update_first_and_last(&mut first, &mut last, '3');
                    }
                    *state = State::Terminate
                }
                State::F => {
                    *state = get_state_2(c, HashMap::from([('o', State::Fo), ('i', State::Fi)]))
                }
                State::Fo => *state = get_state(c, 'u', State::Fou),
                State::Fou => {
                    if c == 'r' {
                        update_first_and_last(&mut first, &mut last, '4');
                    }
                    *state = State::Terminate
                }
                State::Fi => *state = get_state(c, 'v', State::Fiv),
                State::Fiv => {
                    if c == 'e' {
                        update_first_and_last(&mut first, &mut last, '5');
                    }
                    *state = State::Terminate
                }
                State::S => {
                    *state = get_state_2(c, HashMap::from([('i', State::Si), ('e', State::Se)]))
                }
                State::Si => {
                    if c == 'x' {
                        update_first_and_last(&mut first, &mut last, '6');
                    }
                    *state = State::Terminate
                }
                State::Se => *state = get_state(c, 'v', State::Sev),
                State::Sev => *state = get_state(c, 'e', State::Seve),
                State::Seve => {
                    if c == 'n' {
                        update_first_and_last(&mut first, &mut last, '7');
                    }
                    *state = State::Terminate
                }
                State::E => *state = get_state(c, 'i', State::Ei),
                State::Ei => *state = get_state(c, 'g', State::Eig),
                State::Eig => *state = get_state(c, 'h', State::Eigh),
                State::Eigh => {
                    if c == 't' {
                        update_first_and_last(&mut first, &mut last, '8');
                    }
                    *state = State::Terminate
                }
                State::N => *state = get_state(c, 'i', State::Ni),
                State::Ni => *state = get_state(c, 'n', State::Nin),
                State::Nin => {
                    if c == 'e' {
                        update_first_and_last(&mut first, &mut last, '9');
                    }
                    *state = State::Terminate
                }
                State::Digit => {
                    if lastc.is_none() {
                        panic!("Last c is none")
                    }
                    update_first_and_last(&mut first, &mut last, lastc.unwrap());
                    *state = State::Terminate
                }
            }
        }
        lastc = Some(c);
        states.retain(|(state, _)| *state != State::Terminate);
    }
    if let (Some(x), Some(y)) = (first, last) {
        return Some(x * 10 + y);
    }
    None
}

pub fn find_number_part_1(s: &str) -> Option<u64> {
    let mut first: Option<u64> = None;
    let mut last: Option<u64> = None;
    for c in s.chars() {
        if c.is_ascii_digit() {
            update_first_and_last(&mut first, &mut last, c);
        }
    }
    if let (Some(x), Some(y)) = (first, last) {
        return Some(x * 10 + y);
    }
    None
}

pub fn process( file: &str, mut algorithm: impl FnMut(&str)-> Option<u64>){
    let mut f = BufReader::new(File::open(file).expect("open failed"));
    let mut buf: Vec<u8> = Vec::<u8>::new();
    let mut total: u64 = 0;
    while f.read_until(b'\n', &mut buf).expect("read_until failed") != 0 {
        let s = String::from_utf8(buf).expect("from_utf8 failed");
        if let Some(x) = algorithm(&s) {
            //println!("{}:{}", s.trim(), x);
            total += x
        } else {
            panic!("find_number returned None")
        }
        buf = s.into_bytes();
        buf.clear();
    }
    println!("Total is: {}", total)   
}

