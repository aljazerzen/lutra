fn main() {
    let source = r#"
        type x = {int, a = text, b = {c = {d = [bool], e = float}, float}}

        type y = int
        
        type z = [int]
    "#;

    print!("{}", lutra_codegen::codegen(source).unwrap());
}
