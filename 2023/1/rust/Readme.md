I solved the first part by just identifying digits using the char.is_ascii_digit function.

The second was solved by creating a state machine that started on every char. That way we could keep track of all possible instances of numbers in digital or string form, even if they overlap.

I think that would be faster if we just use a regex, But building the state machine is much cooler.

