open! Base

let expand ~loc ~pattern ~expression ~attributes =
  let parsed_let_expression =
    Quick_test_let_expression.parse
      ~loc
      ~pattern
      ~expression
      ~value_binding_attributes:attributes
  in
  Quick_test_let_expression.expand parsed_let_expression
;;

let enclose_impl loc = Quick_test_header_footer.expand_enclose_impl loc
