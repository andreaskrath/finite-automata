use std::{collections::HashMap, rc::Rc};

// region: Types

/// A wrapper type for the HashMap containing the transitions of the Dfa.
type TransitionTable = HashMap<(char, Rc<str>), Rc<str>>;

// endregion

// region: Deterministic Finite Automata

/// A deterministic finite automata.
///
/// # Usage
/// A valid Dfa must have values for the following properties:
/// - alphabet (a slice of ASCII characters)
/// - initial state
/// - accept states
/// - states
///
/// Use the [`builder`] method to create and define the Dfa.
///
/// Alternatively, the [`DfaBuilder`] struct can also be used for creating a Dfa.
///
/// # Examples
/// ```
/// use finite_automata::dfa::{Dfa, DfaState};
/// // the Dfa defined below accepts any
/// // input string with the suffix "ab"
/// let mut dfa = Dfa::builder()
///     .alphabet(&['a', 'b'])
///     .initial_state("q1")
///     .accept_states(&["q3"])
///     .states(&[
///         DfaState::new("q1")
///             .transition('a', "q2")
///             .transition('b', "q1"),
///         DfaState::new("q2")
///             .transition('a', "q2")
///             .transition('b', "q3"),
///         DfaState::new("q3")
///             .transition('a', "q2")
///             .transition('b', "q1"),
///     ])
///     .build()
///     .expect("failed to build dfa");
///
/// // the check_input method returns an error
/// // upon encountering invalid input characters
/// assert!(dfa.check_input("ab").is_ok_and(|b| b == true));
/// assert!(dfa.check_input("aba").is_ok_and(|b| b == false));
/// ```
pub struct Dfa {
    alphabet: Box<[char]>,
    transitions: TransitionTable,
    initial_state: Rc<str>,
    accept_states: Box<[Rc<str>]>,
    current_state: Rc<str>,
    state_order: Vec<Rc<str>>,
}

impl<'a> Dfa {
    // region: Public API

    /// Returns a `DfaBuilder`, which is used to create a Dfa.
    ///
    /// This method exists for convenience, and simply calls the [`DfaBuilder::new`] function.
    ///
    /// **For information about the builder and examples of its usage, see the documentation of the [`DfaBuilder`]**
    pub fn builder() -> DfaBuilder<'a> {
        DfaBuilder::new()
    }

    /// Advances the Dfa with the provided input char.
    ///
    /// Upon a successful advance, the name of the new state is contained within the `Ok` enum.
    ///
    /// # Errors
    /// Returns an `Err` variant if the input character is not a part of the defined alphabet.
    ///
    /// # Examples
    /// ```
    /// # use finite_automata::dfa::{Dfa, DfaState};
    /// # let mut dfa = Dfa::builder()
    /// #     .alphabet(&['a', 'b'])
    /// #     .initial_state("q1")
    /// #     .accept_states(&["q3"])
    /// #     .states(&[
    /// #         DfaState::new("q1")
    /// #             .transition('a', "q2")
    /// #             .transition('b', "q1"),
    /// #         DfaState::new("q2")
    /// #             .transition('a', "q2")
    /// #             .transition('b', "q3"),
    /// #         DfaState::new("q3")
    /// #             .transition('a', "q2")
    /// #             .transition('b', "q1"),
    /// #     ])
    /// #     .build()
    /// #     .expect("failed to build dfa");
    /// // the dfa in this example recognizes any input sequence with the suffix "ab"
    /// assert!(!dfa.in_accept_state()); // not starting in accept state
    /// dfa.advance('a').expect("failed to advance input");
    ///
    /// assert!(!dfa.in_accept_state()); // first input does not advance to accept state
    /// dfa.advance('b').expect("failed to advance input");
    ///
    /// assert!(dfa.in_accept_state()); // dfa is now in accept state
    /// dfa.advance('a').expect("failed to advance input");
    ///
    /// assert!(!dfa.in_accept_state()); // suffix is no longer "ab"
    /// ```
    #[inline]
    pub fn advance(&mut self, input: char) -> Result<&str, DfaError<'a>> {
        if !self.alphabet.contains(&input) {
            return Err(DfaError::InvalidInputChar(input));
        }
        self.state_order.push(Rc::clone(&self.current_state));

        // all possible transitions are checked during the build,
        // as such it should be impossible to have an invalid transition at this point
        let new_state_rc = self
            .transitions
            .get(&(input, Rc::clone(&self.current_state)))
            .unwrap();
        self.current_state = Rc::clone(new_state_rc);
        Ok(new_state_rc.as_ref())
    }

    /// Checks if the provided input is accepted by the defined Dfa.
    /// The `Ok` variant contains a bool, where *true* means the input was accepted,
    /// and *false* means the input was declined.
    ///
    /// This method resets the state order before computing the input.
    ///
    /// **This method is self-contained, meaning it starts in the `initial state`.**
    ///
    /// Should you wish to check an input without this behaviour, then use [`check_input_from_current`] method instead.
    ///
    /// # Errors
    /// Returns an `Err` variant if any character in the input string is not a part of the defined alphabet.
    pub fn check_input(&mut self, input_str: &str) -> Result<bool, DfaError<'a>> {
        self.reset_state_order();
        self.current_state = Rc::clone(&self.initial_state);

        for c in input_str.chars() {
            self.advance(c)?;
        }

        Ok(self.in_accept_state())
    }

    /// This method is a variant of [`check_input`] and behaves the same, as such its documentation should
    /// be sought out for information regarding its use.
    ///
    /// The following expections apply:
    /// - This method starts from the current state of the Dfa, not the initial state.
    /// - This method does not reset the state order
    pub fn check_input_from_current(&mut self, input_str: &str) -> Result<bool, DfaError<'a>> {
        for c in input_str.chars() {
            self.advance(c)?;
        }

        Ok(self.in_accept_state())
    }

    /// Returns the order in which the Dfa visited its states.
    ///
    /// This method starts by appending the current state of the Dfa to the state order,
    /// meaning the returned slice will always contain at least one state.
    #[inline]
    pub fn state_order(&mut self) -> Box<[&str]> {
        self.state_order.push(Rc::clone(&self.current_state));
        self.state_order.iter().map(|s| s.as_ref()).collect()
    }

    /// Resets the recorded state order of the Dfa.
    #[inline]
    pub fn reset_state_order(&mut self) {
        self.state_order.clear();
    }

    /// Checks if the current state of the Dfa is an accept state.
    #[inline]
    pub fn in_accept_state(&self) -> bool {
        self.accept_states.contains(&self.current_state)
    }

    /// Resets the current state of the Dfa to its initial state.
    #[inline]
    pub fn reset_state(&mut self) {
        self.current_state = Rc::clone(&self.initial_state);
    }

    // endregion

    // region: Internal Implementation

    // endregion
}

// endregion

// region: Deterministic Finite Automata Builder

#[derive(Debug, Default)]
pub struct DfaBuilder<'a> {
    alphabet: Option<&'a [char]>,
    initial_state: Option<&'a str>,
    accept_states: Option<&'a [&'a str]>,
    states: Option<&'a [DfaState<'a>]>,
}

impl<'a> DfaBuilder<'a> {
    // region: Builder API

    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the alphabet of the Dfa; the alphabet must only include ASCII characters.
    ///
    /// This method is a part of the builder pattern for the Dfa construct;
    /// the following methods must be called to build a valid Dfa:
    /// - [`alphabet`]
    /// - [`initial_state`]
    /// - [`accept_states`]
    /// - [`states`]
    ///
    /// After which the Dfa is validated and constructed via the [`build`] method.
    ///
    /// # Examples
    /// ```
    /// use finite_automata::dfa::Dfa;
    /// let mut dfa = Dfa::builder().alphabet(&['a', 'b', 'c']);
    /// ```
    pub fn alphabet(mut self, slice: &'a [char]) -> Self {
        self.alphabet = Some(slice);
        self
    }

    pub fn initial_state(mut self, name: &'a str) -> Self {
        self.initial_state = Some(name);
        self
    }

    pub fn accept_states(mut self, states: &'a [&'a str]) -> Self {
        self.accept_states = Some(states);
        self
    }

    pub fn states(mut self, states: &'a [DfaState<'a>]) -> Self {
        self.states = Some(states);
        self
    }

    pub fn build(self) -> Result<Dfa, DfaError<'a>> {
        use DfaError::*;

        let state_map: HashMap<&str, Rc<str>> = match self.states {
            Some(slice) => {
                if slice.is_empty() {
                    return Err(EmptyStates);
                }

                let mut map = HashMap::new();
                for state in slice.iter() {
                    map.insert(state.name.as_ref(), Rc::clone(&state.name));
                }

                map
            }
            None => return Err(StatesNotDefined),
        };

        let start_state = self.generate_initial_state(&state_map)?;
        Ok(Dfa {
            alphabet: self.generate_alphabet()?,
            transitions: self.generate_transitions(&state_map)?,
            initial_state: Rc::clone(&start_state),
            accept_states: self.generate_accept_states(&state_map)?,
            current_state: start_state,
            state_order: Vec::new(),
        })
    }

    // endregion

    // region: Internal Builder Methods

    #[inline]
    fn generate_transitions(
        &self,
        state_map: &HashMap<&str, Rc<str>>,
    ) -> Result<TransitionTable, DfaError<'a>> {
        use DfaError::*;

        let slice = self.states.unwrap();
        let alphabet_slice = self.alphabet.unwrap();

        // The final transition table used in the Dfa is a HashMap of the following type
        // as such, a tuple of an input char and a current state is provided to yield the
        // next state the Dfa will transition to.
        let mut map: TransitionTable = HashMap::new();

        for state in slice.iter() {
            for c in alphabet_slice.iter() {
                // a Dfa requires that each state has a valid transition for every possible input
                if state.transitions.get(c).is_none() {
                    return Err(StateMissingTransition(state.name.as_ref(), *c));
                }
            }

            for (trans_input, trans_dest) in state.transitions.iter() {
                // all transitions must be defined with symbols that are a part of the defined alphabet
                if !alphabet_slice.contains(trans_input) {
                    return Err(CharNotInAlphabet(*trans_input));
                }

                match state_map.get(trans_dest) {
                    Some(state_name_rc) => {
                        map.insert(
                            (*trans_input, Rc::clone(&state.name)),
                            Rc::clone(state_name_rc),
                        );
                    }
                    // transition destinations are only allowed within the defined states
                    None => return Err(InvalidTransitionDestination(trans_dest)),
                }
            }
        }

        Ok(map)
    }

    #[inline]
    fn generate_alphabet(&self) -> Result<Box<[char]>, DfaError<'a>> {
        use DfaError::*;

        match self.alphabet {
            Some(slice) => {
                for c in slice.iter() {
                    if !c.is_ascii() {
                        return Err(NonAsciiChar(*c));
                    }
                }

                Ok(slice.to_vec().into_boxed_slice())
            }
            None => Err(AlphabetNotDefined),
        }
    }

    #[inline]
    fn generate_initial_state(
        &self,
        state_map: &HashMap<&str, Rc<str>>,
    ) -> Result<Rc<str>, DfaError<'a>> {
        use DfaError::*;

        match self.initial_state {
            Some(initial_state_name) => {
                let Some(initial_state_rc) = state_map.get(initial_state_name) else {
                    return Err(InvalidInitialState(initial_state_name));
                };

                Ok(Rc::clone(initial_state_rc))
            }
            None => Err(InitialStateNotDefined),
        }
    }

    #[inline]
    fn generate_accept_states(
        &self,
        state_map: &HashMap<&str, Rc<str>>,
    ) -> Result<Box<[Rc<str>]>, DfaError<'a>> {
        use DfaError::*;

        match self.accept_states {
            Some(slice) => {
                let mut rc_vec = Vec::new();

                for state_name in slice {
                    match state_map.get(state_name) {
                        Some(state_name_rc) => rc_vec.push(Rc::clone(state_name_rc)),
                        None => return Err(InvalidAcceptState(state_name)),
                    }
                }

                Ok(rc_vec.into_boxed_slice())
            }
            None => Err(AcceptStatesNotDefined),
        }
    }

    // endregion
}

// endregion

// region: Deterministic Finite Automata State

#[derive(Debug)]
pub struct DfaState<'a> {
    name: Rc<str>,
    transitions: HashMap<char, &'a str>,
}

impl<'a> DfaState<'a> {
    pub fn new(name: &str) -> Self {
        Self {
            name: Rc::from(name),
            transitions: HashMap::new(),
        }
    }

    pub fn transition(mut self, input: char, new_state: &'a str) -> Self {
        self.transitions.insert(input, new_state);
        self
    }
}

// endregion

// region: Deterministic Finite Automata Errors

use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum DfaError<'a> {
    #[error("the alphabet for the dfa was not defined")]
    AlphabetNotDefined,
    #[error("the states of the dfa were not defined")]
    StatesNotDefined,
    #[error("the initial state was not defined")]
    InitialStateNotDefined,
    #[error("the accept states were not defined")]
    AcceptStatesNotDefined,
    #[error("the provided states slice was empty")]
    EmptyStates,
    #[error("'{0}' is not an ASCII character")]
    NonAsciiChar(char),
    #[error("'{0}' is not a part of the defined alphabet, and cannot be used in a transition")]
    CharNotInAlphabet(char),
    #[error("the state '{0}' does not have a transition for the alphabet character '{1}'")]
    StateMissingTransition(&'a str, char),
    #[error("'{0}' is not one of the defined states, and cannot be used as a destination for a transition")]
    InvalidTransitionDestination(&'a str),
    #[error("'{0}' is not one of the defined states, and cannot be used as the initial state")]
    InvalidInitialState(&'a str),
    #[error("'{0}' is not one of the defined states, and cannot be used as an accept state")]
    InvalidAcceptState(&'a str),
    #[error("'{0}' is not a part of the defined alphabet")]
    InvalidInputChar(char),
}

// endregion

// region: Tests

// region: DfaBuilder
#[cfg(test)]
mod dfa_builder {
    use super::{Dfa, DfaError, DfaState};

    // in the following tests the states are build seperately, as the value is dropped during build
    // and as no expect or unwrap is called afterwards, it causes a compile error

    #[test]
    fn alphabet_not_defined_error() {
        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q1")];

        let dfa = Dfa::builder()
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::AlphabetNotDefined));
    }

    #[test]
    fn states_not_defined_error() {
        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::StatesNotDefined));
    }

    #[test]
    fn initial_state_not_defined_error() {
        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q1")];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .accept_states(&["q1"])
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::InitialStateNotDefined));
    }

    #[test]
    fn accept_states_not_defined_error() {
        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q1")];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::AcceptStatesNotDefined));
    }

    #[test]
    fn empty_states_error() {
        let states = [];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::EmptyStates));
    }

    #[test]
    fn state_missing_transition_error() {
        let states = [DfaState::new("q1").transition('a', "q1")];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::StateMissingTransition("q1", 'b')));
    }

    #[test]
    fn char_not_in_alphabet_error() {
        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q1")
            .transition('c', "q1")];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::CharNotInAlphabet('c')));
    }

    #[test]
    fn invalid_transition_destination_error() {
        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q2")];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::InvalidTransitionDestination("q2")));
    }

    #[test]
    fn non_ascii_char_error() {
        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q1")];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'ø'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::NonAsciiChar('ø')));
    }

    #[test]
    fn invalid_initial_state() {
        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q1")];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q2")
            .accept_states(&["q1"])
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::InvalidInitialState("q2")));
    }

    #[test]
    fn invalid_accept_state() {
        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q1")];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1", "q2"])
            .states(&states)
            .build();

        assert!(dfa.is_err_and(|e| e == DfaError::InvalidAcceptState("q2")));
    }

    #[test]
    fn intended_use() {
        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q1")];

        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&states)
            .build();

        assert!(dfa.is_ok());
    }

    #[test]
    fn correct_initial_state() {
        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&[DfaState::new("q1")
                .transition('a', "q1")
                .transition('b', "q1")])
            .build()
            .expect("failed to build dfa");

        let actual = dfa.initial_state.as_ref();
        let expected = "q1";
        assert_eq!(actual, expected);
    }

    #[test]
    fn correct_current_state() {
        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&[DfaState::new("q1")
                .transition('a', "q1")
                .transition('b', "q1")])
            .build()
            .expect("failed to build dfa");

        let actual = dfa.current_state.as_ref();
        let expected = "q1";
        assert_eq!(actual, expected);
    }

    #[test]
    fn correct_accept_states() {
        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1", "q2"])
            .states(&[
                DfaState::new("q1")
                    .transition('a', "q1")
                    .transition('b', "q1"),
                DfaState::new("q2")
                    .transition('a', "q1")
                    .transition('b', "q2"),
            ])
            .build()
            .expect("failed to build dfa");

        let actual_one = dfa.accept_states[0].as_ref();
        let expected_one = "q1";
        let actual_two = dfa.accept_states[1].as_ref();
        let expected_two = "q2";
        assert_eq!(actual_one, expected_one);
        assert_eq!(actual_two, expected_two);
    }
}
// endregion

#[cfg(test)]
mod public_api {
    use crate::dfa::{Dfa, DfaError, DfaState};

    #[test]
    fn invalid_input_char_error() {
        // for some reason it is impossible to get lifetimes to work with
        // error propagation and defining a lifetime variable
        // as such, expect was used as a last resort

        let states = [DfaState::new("q1")
            .transition('a', "q1")
            .transition('b', "q1")];

        let mut dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&states)
            .build()
            .expect("failed to build dfa");

        assert!(dfa
            .advance('c')
            .is_err_and(|e| e == DfaError::InvalidInputChar('c')));
    }

    #[test]
    fn in_accept_state_true() {
        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&[DfaState::new("q1")
                .transition('a', "q1")
                .transition('b', "q1")])
            .build()
            .expect("failed to build dfa");

        assert!(dfa.in_accept_state());
    }

    #[test]
    fn in_accept_state_false() {
        let dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q2"])
            .states(&[
                DfaState::new("q1")
                    .transition('a', "q2")
                    .transition('b', "q2"),
                DfaState::new("q2")
                    .transition('a', "q1")
                    .transition('b', "q1"),
            ])
            .build()
            .expect("failed to build dfa");

        assert!(!dfa.in_accept_state());
    }

    #[test]
    fn in_accept_state_true_after_advance() {
        let mut dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q2"])
            .states(&[
                DfaState::new("q1")
                    .transition('a', "q2")
                    .transition('b', "q2"),
                DfaState::new("q2")
                    .transition('a', "q1")
                    .transition('b', "q1"),
            ])
            .build()
            .expect("failed to build dfa");

        dfa.advance('a').expect("failed to advance");
        assert!(dfa.in_accept_state());
    }

    #[test]
    fn in_accept_state_false_after_advance() {
        let mut dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q1"])
            .states(&[
                DfaState::new("q1")
                    .transition('a', "q2")
                    .transition('b', "q2"),
                DfaState::new("q2")
                    .transition('a', "q1")
                    .transition('b', "q1"),
            ])
            .build()
            .expect("failed to build dfa");

        dfa.advance('a').expect("failed to advance");
        assert!(!dfa.in_accept_state());
    }

    #[test]
    fn check_input_accepts() {
        // dfa should accept any input with "ab" suffix
        let mut dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q3"])
            .states(&[
                DfaState::new("q1")
                    .transition('a', "q2")
                    .transition('b', "q1"),
                DfaState::new("q2")
                    .transition('a', "q2")
                    .transition('b', "q3"),
                DfaState::new("q3")
                    .transition('a', "q2")
                    .transition('b', "q1"),
            ])
            .build()
            .expect("failed to build dfa");

        let inputs = &["ab", "abab", "aaaaaab", "bbbbbbab"];

        for input in inputs {
            assert!(dfa.check_input(input).expect("failed to check input"));
        }
    }

    #[test]
    fn check_input_declines() {
        // dfa should accept any input with "ab" suffix
        let mut dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q3"])
            .states(&[
                DfaState::new("q1")
                    .transition('a', "q2")
                    .transition('b', "q1"),
                DfaState::new("q2")
                    .transition('a', "q2")
                    .transition('b', "q3"),
                DfaState::new("q3")
                    .transition('a', "q2")
                    .transition('b', "q1"),
            ])
            .build()
            .expect("failed to build dfa");

        let inputs = &["aba", "abba", "bbbbbbbbaa", "aaaaaaaaaaaaaaaaaaba"];

        for input in inputs {
            assert!(!dfa.check_input(input).expect("failed to check input"));
        }
    }

    #[test]
    fn advance_changing_state() {
        let mut dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q2"])
            .states(&[
                DfaState::new("q1")
                    .transition('a', "q2")
                    .transition('b', "q1"),
                DfaState::new("q2")
                    .transition('a', "q2")
                    .transition('b', "q2"),
            ])
            .build()
            .expect("failed to build dfa");

        dfa.advance('a').expect("failed to advance");
        let actual = dfa.current_state.as_ref();
        let expected = "q2";
        assert_eq!(actual, expected);
    }

    #[test]
    fn advance_not_changing_state() {
        let mut dfa = Dfa::builder()
            .alphabet(&['a', 'b'])
            .initial_state("q1")
            .accept_states(&["q2"])
            .states(&[
                DfaState::new("q1")
                    .transition('a', "q2")
                    .transition('b', "q1"),
                DfaState::new("q2")
                    .transition('a', "q2")
                    .transition('b', "q2"),
            ])
            .build()
            .expect("failed to build dfa");

        dfa.advance('b').expect("failed to advance");
        let actual = dfa.current_state.as_ref();
        let expected = "q1";
        assert_eq!(actual, expected);
    }
}

// endregion
