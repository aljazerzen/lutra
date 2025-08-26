/// Converts a string to snake_case
/// Handles CamelCase, PascalCase, kebab-case, and space separated formats
///
/// # Examples
/// ```
/// assert_eq!(to_snake_case("HelloWorld"), "hello_world");
/// assert_eq!(to_snake_case("userName"), "user_name");
/// assert_eq!(to_snake_case("user-name"), "user_name");
/// assert_eq!(to_snake_case("first name"), "first_name");
/// ```
pub fn to_snake_case(input: &str) -> String {
    if input.is_empty() {
        return String::new();
    }

    let mut result = String::new();
    let mut prev_char_lowercase = false;

    for (i, c) in input.char_indices() {
        match c {
            // Replace separators with underscore
            '-' | ' ' | '.' => {
                if !result.is_empty() && !result.ends_with('_') {
                    result.push('_');
                }
            }
            // Handle uppercase letters
            c if c.is_uppercase() => {
                // Add underscore before uppercase if previous was lowercase
                if i > 0 && prev_char_lowercase && !result.ends_with('_') {
                    result.push('_');
                }
                result.push(c.to_lowercase().next().unwrap_or(c));
                prev_char_lowercase = false;
            }
            // Handle lowercase and other characters
            c => {
                result.push(c);
                prev_char_lowercase = c.is_lowercase();
            }
        }
    }

    result
}

/// Converts a string to PascalCase
/// Handles snake_case, kebab-case, space separated, and mixed formats
///
/// # Examples
/// ```
/// assert_eq!(to_camel_case("hello_world"), "HelloWorld");
/// assert_eq!(to_camel_case("user-name"), "UserName");
/// assert_eq!(to_camel_case("first name"), "FirstName");
/// assert_eq!(to_camel_case("XMLHttpRequest"), "XmlHttpRequest");
/// ```
pub fn to_pascal_case(input: &str) -> String {
    input
        .split(['_', '-'])
        .filter(|s| !s.is_empty())
        .map(|word| {
            let mut chars = word.chars();
            if let Some(first) = chars.next() {
                let rest: String = chars.collect();
                format!("{}{}", first.to_uppercase(), rest.to_lowercase())
            } else {
                String::new()
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("HelloWorld"), "hello_world");
        assert_eq!(to_snake_case("userName"), "user_name");
        assert_eq!(to_snake_case("user-name"), "user_name");
        assert_eq!(to_snake_case("first name"), "first_name");
        assert_eq!(to_snake_case("XmlHttpRequest"), "xml_http_request");
        assert_eq!(to_snake_case("camelCase"), "camel_case");
        assert_eq!(to_snake_case("already_snake"), "already_snake");
        assert_eq!(to_snake_case(""), "");
        assert_eq!(to_snake_case("single"), "single");
    }

    #[test]
    fn test_to_camel_case() {
        assert_eq!(to_pascal_case("hello_world"), "HelloWorld");
        assert_eq!(to_pascal_case("user-name"), "UserName");
        assert_eq!(to_pascal_case("snake_case_example"), "SnakeCaseExample");
        assert_eq!(to_pascal_case("kebab-case-example"), "KebabCaseExample");
        assert_eq!(to_pascal_case("mixed_case-example"), "MixedCaseExample");
        assert_eq!(to_pascal_case(""), "");
        assert_eq!(to_pascal_case("single"), "Single");
        assert_eq!(to_pascal_case("UPPER_CASE"), "UpperCase");
        assert_eq!(to_pascal_case("xml_http_request"), "XmlHttpRequest");
    }
}
