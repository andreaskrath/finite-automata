// /// Specifies the required methods of an FA.
// pub trait FiniteAutomata {
//     /// Advances the FA by a single input character.
//     fn advance_with(&mut self, input: char);

//     /// Checks the input string character by character, against the defined FA.
//     fn check_input(&mut self, input: &str);
// }

// pub trait AutomataBuilder<'a> {
//     /// Specifies the alphabet of the FA.
//     fn alphabet<I>(self, alphabet: I) -> Self
//     where
//         I: IntoIterator<Item = char>;

//     /// Specifies the initial state of the FA.
//     fn initial_state(self, state_name: &str) -> Self;

//     /// Specifies the accept state(s) of the FA.
//     fn accept_states<I>(self, accept_states: I) -> Self
//     where
//         I: IntoIterator<Item = &'a str>;

//     /// Specifies the states of the FA, and their transitions.
//     fn states<I>(self, states: I) -> Self
//     where
//         I: IntoIterator<Item = Box<dyn State>>;

//     /// Builds the FA.
//     fn build(self) -> dyn FiniteAutomata;
// }

// pub trait State {
//     fn add_transition(self, input_char: char, next_state: &str) -> Self;
// }
