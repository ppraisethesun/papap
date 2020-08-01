defmodule Nimble do
  import NimbleParsec

  defmodule Base do
    def ignore_ws(combinator) do
      ignore(combinator, choice([repeat(ascii_char([?\s])), empty()]))
    end
  end

  opening_bracket = string("(") |> Base.ignore_ws() |> label("opening bracket")
  closing_bracket = string(")") |> Base.ignore_ws() |> label("closing bracket")

  add_sub = utf8_string([?+, ?-], 1) |> Base.ignore_ws() |> label("operator")
  mul_div = utf8_string([?*, ?/], 1) |> Base.ignore_ws() |> label("operator")

  func_opening_bracket = string("{") |> Base.ignore_ws() |> label("function opening bracket")
  func_closing_bracket = string("}") |> Base.ignore_ws() |> label("function closing bracket")
  func_args_separator = string(",") |> Base.ignore_ws() |> label("args separator")

  integer =
    integer(min: 1)
    |> label("integer")
    |> lookahead_not(string("."))
    |> map({__MODULE__, :wrap, [:integer]})
    |> Base.ignore_ws()

  float =
    integer(min: 1)
    |> label("whole part")
    |> label(string("."), "decimal separator")
    |> label(integer(min: 1), "decimal part")
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

  function_call =
    ident
    |> ignore(func_opening_bracket)
    |> concat(func_call_args)
    |> ignore(func_closing_bracket)
    |> wrap()
    |> map({__MODULE__, :wrap, [:function_call]})

  expression_in_brackets =
    ignore(opening_bracket)
    |> parsec(:expression)
    |> ignore(closing_bracket)

  factor =
    choice([
      number,
      variable,
      expression_in_brackets,
      function_call
    ])

  defparsec(
    :term,
    choice([
      factor
      |> concat(mul_div)
      |> parsec(:term)
      |> wrap()
      |> map({__MODULE__, :wrap, [:operator]}),
      factor
    ])
  )

  defparsec(
    :expression,
    choice([
      parsec(:term)
      |> concat(add_sub)
      |> parsec(:expression)
      |> wrap()
      |> map({__MODULE__, :wrap, [:operator]}),
      parsec(:term)
    ])
    |> label("expression")
  )

  def wrap(term, :integer), do: %{data: [integer: term]}
  def wrap(term, :variable), do: %{data: [variable: term]}

  def wrap([a, op, b], :operator) do
    %{
      data: [operator: op],
      args: [a, b]
    }
  end

  def wrap([func_name, args], :function_call) do
    %{
      data: [function: func_name],
      args: args
    }
  end
end
