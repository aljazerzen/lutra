#[cfg(not(feature = "std"))]
use alloc::string::String;

use core::fmt;

pub fn escape(string: &str, quote: char) -> EscapeQuotedString<'_> {
    EscapeQuotedString { string, quote }
}

pub struct EscapeQuotedString<'a> {
    string: &'a str,
    quote: char,
}

impl fmt::Display for EscapeQuotedString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // EscapeQuotedString doesn't know which mode of escape was
        // chosen by the user. So this code must to correctly display
        // strings without knowing if the strings are already escaped
        // or not.
        //
        // If the quote symbol in the string is repeated twice, OR, if
        // the quote symbol is after backslash, display all the chars
        // without any escape. However, if the quote symbol is used
        // just between usual chars, `fmt()` should display it twice."
        //
        // The following table has examples
        //
        // | original query | mode      | AST Node                                           | serialized   |
        // | -------------  | --------- | -------------------------------------------------- | ------------ |
        // | `"A""B""A"`    | no-escape | `DoubleQuotedString(String::from("A\"\"B\"\"A"))`  | `"A""B""A"`  |
        // | `"A""B""A"`    | default   | `DoubleQuotedString(String::from("A\"B\"A"))`      | `"A""B""A"`  |
        // | `"A\"B\"A"`    | no-escape | `DoubleQuotedString(String::from("A\\\"B\\\"A"))`  | `"A\"B\"A"`  |
        // | `"A\"B\"A"`    | default   | `DoubleQuotedString(String::from("A\"B\"A"))`      | `"A""B""A"`  |
        let quote = self.quote;
        let mut previous_char = char::default();
        let mut start_idx = 0;
        let mut peekable_chars = self.string.char_indices().peekable();
        while let Some(&(idx, ch)) = peekable_chars.peek() {
            match ch {
                char if char == quote => {
                    if previous_char == '\\' {
                        // the quote is already escaped with a backslash, skip
                        peekable_chars.next();
                        continue;
                    }
                    peekable_chars.next();
                    match peekable_chars.peek() {
                        Some((_, c)) if *c == quote => {
                            // the quote is already escaped with another quote, skip
                            peekable_chars.next();
                        }
                        _ => {
                            // The quote is not escaped.
                            // Including idx in the range, so the quote at idx will be printed twice:
                            // in this call to write_str() and in the next one.
                            f.write_str(&self.string[start_idx..=idx])?;
                            start_idx = idx;
                        }
                    }
                }
                _ => {
                    peekable_chars.next();
                }
            }
            previous_char = ch;
        }
        f.write_str(&self.string[start_idx..])?;
        Ok(())
    }
}
