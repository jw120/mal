// Reader implemented following mal instructions

use regex::Regex;

// Convert source code into a vector of tokens
pub fn tokenize(source: &str) -> Vec<String> {
    let re = Regex::new(
        r###"(?x)
        [\s,]* # Whitespace and commas not captured
        ( # Capture one of these alternatives
            ~@|                 # Special two-character ~@ token
            [\[\]{}()'`~^@]|    # Single character special token
            "(?:\\.|[^\\"])*"?| # From double-quote to next un-escaped double-quote
            ;.*|                # Anything starting with ;
            [^\s\[\]{}('"`,;)]* # Zero-or-more non-special characters
        )
        "###,
    )
    .unwrap();
    let mut tokens = vec![];
    for (_, [token]) in re.captures_iter(source).map(|c| c.extract()) {
        tokens.push(token.to_string())
    }
    tokens
}
