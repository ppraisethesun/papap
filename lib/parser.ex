defmodule Parser do
  @type succ(t) :: {:ok, t, binary}
  @type fail :: {:error, term}
  @type result(t) :: succ(t) | fail

  def parse(input) do
    list_of(token(ident()), token(char(?,))).(input)
  end

  # expression
  #     term
  #     | expression '+' term
  #     | expression '-' term
  #     ;

  # term
  #     factor
  #     | term '*' factor
  #     | term '/' factor

  # factor
  #     var
  #     | integer
  #     | float
  #     | '(' expression ')'
  #     | func_name '(' expression (',' expression)* ')'
  @table :parsers
  def init do
    :ets.new(@table, [:named_table, :set])
  end

  def function do
    case :ets.lookup(@table, :function) do
      [] ->
        parser =
          sequence([
            token(ident()),
            token(char(?\()),
            any([
              lazy(fn -> function() end),
              token(ident())
            ])
            |> list_of(char(?,)),
            token(char(?\)))
          ])
          |> map(fn [name, _, args, _] ->
            %{
              function: name,
              args: args
            }
          end)
          |> IO.inspect(label: "function")

        :ets.insert(@table, {:function, parser})
        parser

      [{:function, parser}] ->
        parser
    end
  end

  def lazy(combinator) do
    fn input ->
      combinator.().(input)
    end
  end

  def list_of(parser, separator) do
    sequence([
      parser,
      repeat(sequence([separator, parser]))
    ])
    |> map(fn [head, rest] ->
      tail = Enum.map(rest, fn [_, term] -> term end)
      [head | tail]
    end)
  end

  def sequence(parsers) do
    fn input ->
      case parsers do
        [] ->
          {:ok, [], input}

        [parser | rest_parsers] ->
          with {:ok, first_term, rest_input} <- parser.(input),
               {:ok, rest_terms, rest_input} <- sequence(rest_parsers).(rest_input) do
            {:ok, [first_term | rest_terms], rest_input}
          end
      end
    end
  end

  # skip whitespaces
  def token(parser) do
    sequence([
      [char(?\s), char(?\n)] |> any() |> repeat(),
      parser,
      [char(?\s), char(?\n)] |> any() |> repeat()
    ])
    |> map(fn [_, result, _] -> result end)
  end

  def ident do
    ident_char()
    |> repeat()
    |> satisfy(&(&1 != []))
    |> map(&to_string/1)
  end

  def ident_char do
    fn input ->
      any([
        digit(),
        ascii_letter(),
        char("_")
      ]).(input)
    end
  end

  def repeat(parser) do
    fn input ->
      case parser.(input) do
        {:error, _err} ->
          {:ok, [], input}

        {:ok, first_term, rest_input} ->
          {:ok, other_terms, rest_input} = repeat(parser).(rest_input)
          {:ok, [first_term | other_terms], rest_input}
      end
    end
  end

  def any(parsers) do
    fn input ->
      case parsers do
        [] ->
          {:error, "no parsers"}

        [parser | rest] ->
          with {:error, _err} <- parser.(input) do
            any(rest).(input)
          end
      end
    end
  end

  def digit do
    satisfy(char(), fn char -> char in ?0..?9 end)
  end

  def ascii_letter do
    satisfy(char(), fn char -> char in ?A..?Z or char in ?a..?z end)
  end

  def char(expected) do
    satisfy(char(), fn char -> char == expected end)
  end

  def map(parser, fun) do
    fn input ->
      with {:ok, term, rest} <- parser.(input) do
        {:ok, fun.(term), rest}
      end
    end
  end

  def satisfy(parser, fun) do
    fn input ->
      with {:ok, term, rest} <- parser.(input) do
        if fun.(term),
          do: {:ok, term, rest},
          else: {:error, "NO"}
      end
    end
  end

  def char do
    fn input ->
      case input do
        "" -> {:error, :eol}
        <<char::utf8, rest::binary>> -> {:ok, char, rest}
      end
    end
  end
end
