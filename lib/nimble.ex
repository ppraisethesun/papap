defmodule Nimble do
  import NimbleParsec

  defmodule Base do
    def ignore_ws(combinator) do
      ignore(combinator, choice([repeat(ascii_char([?\s])), empty()]) |> label("whitespace"))
    end
  end

  opening_bracket = string("(") |> byte_offset() |> Base.ignore_ws() |> label("opening bracket")
  closing_bracket = string(")") |> byte_offset() |> Base.ignore_ws() |> label("closing bracket")

  func_opening_bracket =
    string("{") |> byte_offset() |> label("function opening bracket") |> Base.ignore_ws()

  func_closing_bracket =
    string("}") |> byte_offset() |> label("function closing bracket") |> Base.ignore_ws()

  func_args_separator = string(",") |> Base.ignore_ws() |> label("args separator")

  any_operator = utf8_string([?+, ?-, ?*, ?/, ?<, ?>, ?=], min: 1) |> label("any operator")
  any_closing_bracket = utf8_string([?), ?}], min: 1) |> label("any closing bracket")

  add_sub = utf8_string([?+, ?-], 1) |> Base.ignore_ws() |> label("operator")
  mul_div = utf8_string([?*, ?/], 1) |> Base.ignore_ws() |> label("operator")

  integer =
    integer(min: 1)
    |> label("integer")
    |> lookahead_not(string("."))
    |> map({__MODULE__, :wrap, [:integer]})
    |> byte_offset()
    |> Base.ignore_ws()

  float =
    integer(min: 1)
    |> label("whole part")
    |> label(string("."), "decimal separator")
    |> label(integer(min: 1), "decimal part")
    |> byte_offset()
    |> Base.ignore_ws()

  ident =
    choice([
      utf8_string([?a..?z], min: 1),
      utf8_string([?A..?z], min: 1),
      utf8_string([?а..?я], min: 1),
      utf8_string([?А..?Я], min: 1)
    ])
    |> label("ident")

  variable =
    ident
    |> byte_offset()
    |> Base.ignore_ws()
    |> lookahead_not(func_opening_bracket)
    |> map({__MODULE__, :wrap, [:variable]})

  number = choice([integer, float])

  #################################################
  #################################################
  #################################################
  func_call_args =
    parsec(:expression)
    |> repeat(
      ignore(func_args_separator)
      |> lookahead_not(func_closing_bracket)
      |> parsec(:expression)
    )
    |> wrap()
    |> label("function arguments")

  function_name = ident |> label("function name")

  function_call =
    function_name
    |> ignore(func_opening_bracket)
    |> concat(optional(func_call_args))
    |> ignore(func_closing_bracket)
    |> label("function call")
    |> reduce({__MODULE__, :wrap, [:function_call]})
    |> debug()

  if_expr =
    string("?")
    |> Base.ignore_ws()
    |> concat(func_opening_bracket)
    |> parsec(:comparison)
    |> times(
      ignore(func_args_separator)
      |> parsec(:expression)
      |> label("if arg"),
      2
    )
    |> concat(func_closing_bracket)
    |> post_traverse({:if_else, []})
    |> label("if expression")

  def if_else(_, [_, expr2, expr1, condition, _, "?"], context, _, _) do
    node = %{
      data: [function: "?"],
      args: [condition, expr1, expr2]
    }

    {[node], context}
  end

  def if_else(_, [_, _, _, {"{", pos}, "?"], _, _, _) do
    {:error, "no match for { at #{pos}"}
  end

  expression_in_brackets =
    opening_bracket
    |> parsec(:expression)
    |> optional(closing_bracket)
    |> post_traverse({:what_is_this, []})

  def what_is_this(_rest, [_closing, expr, _opening], context, _line, _offset) do
    IO.inspect(expr)
    {[expr], context}
  end

  def what_is_this(_, [_, {paren, position}], _, _, _) do
    {:error, "no match for #{paren} at #{position}"}
  end

  factor =
    choice([
      number,
      variable,
      function_call,
      expression_in_brackets,
      if_expr
    ])
    |> Base.ignore_ws()
    |> label("factor")

  defparsec(
    :term,
    factor
    |> repeat(mul_div |> concat(factor))
    |> reduce({__MODULE__, :left_ass, []})
  )

  expr_start =
    empty()
    |> Base.ignore_ws()
    |> lookahead_not(any_operator)
    |> lookahead_not(any_closing_bracket)

  defparsec(
    :expression,
    expr_start
    |> parsec(:term)
    |> repeat(add_sub |> parsec(:term))
    |> reduce({__MODULE__, :left_ass, []})
  )

  defparsec(:full, parsec(:expression) |> eos())

  comparison_operator =
    choice([
      string("<="),
      string(">="),
      string("<>"),
      string(">"),
      string("<"),
      string("=")
    ])
    |> Base.ignore_ws()
    |> label("comparison operator")

  defparsec(
    :comparison,
    parsec(:expression)
    |> concat(comparison_operator)
    |> parsec(:expression)
    |> lookahead_not(comparison_operator)
    |> reduce({__MODULE__, :wrap_comparison, []})
  )

  def wrap_comparison([a, op, b]) do
    %{
      data: [operator: op],
      args: [a, b]
    }
  end

  @doc """
  wrap left associative operators

  1 - 2 - 3 == (1 - 2) - 3 != 1 - (2 - 3)
  """
  def left_ass([a]), do: a

  def left_ass([a, op, b | rest]) do
    new_a = %{
      data: [operator: op],
      args: [a, b]
    }

    left_ass([new_a | rest])
  end

  def wrap(term, :integer), do: %{data: [integer: term]}
  def wrap(term, :variable), do: %{data: [variable: term]}

  def wrap([func_name, args], :function_call) do
    %{
      data: [function: func_name],
      args: List.flatten(args)
    }
  end

  def wrap([func_name], :function_call) when is_binary(func_name) do
    %{
      data: [function: func_name],
      args: []
    }
  end
end
