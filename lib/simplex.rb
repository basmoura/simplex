require 'matrix'

class Vector
  public :[]=
end

class Simplex
  class UnboundedProblem < StandardError
  end

  def initialize(objetiva, restricoes, constantes)

    # Problem dimensions
    @num_var_normais = restricoes.first.length
    @num_constantes = constantes.length
    @num_var_totais = @num_var_normais + @num_constantes

    # Set up initial matrix A and vectors b, c
    @objetiva = Vector[*objetiva.map { |o| -1*o } + [0]*@num_constantes]
    @retricoes = restricoes.map { |r| Vector[*(r.clone + [0]*@num_constantes)]}
    @constantes = Vector[*constantes.clone]

    unless @retricoes.all? { |r| r.size == @objetiva.size } and @constantes.size == @retricoes.length
      raise ArgumentError, "A quantidade de variáveis deve ser igual para todas as retrições. Caso não exista coloque 0"
    end

    0.upto(@num_constantes - 1) {|i| @retricoes[i][@num_var_normais + i] = 1 }

    # set initial solucao: all non-slack variaveis = 0
    @x          = Vector[*([0]*@num_var_totais)]
    @vars_basicas = (@num_var_normais...@num_var_totais).to_a
    att_solucao
  end

  def solucao
    resolve
    solucao_corrente
  end

  def solucao_corrente
    @x.to_a[0...@num_var_normais]
  end

  def att_solucao
    0.upto(@num_var_totais - 1) {|i| @x[i] = 0 }

    @vars_basicas.each do |basic_var|
      row_with_1 = indice_linha.detect do |row_ix|
        @retricoes[row_ix][basic_var] == 1
      end
      @x[basic_var] = @constantes[row_with_1]
    end
  end

  def resolve
    while pode_melhorar?
      pivo
    end
  end

  def pode_melhorar?
    !!variavel_entrada
  end

  def variaveis
    (0...@objetiva.size).to_a
  end

  def variavel_entrada
    variaveis.select { |var| @objetiva[var] < 0 }.
              min_by { |var| @objetiva[var] }
  end

  def pivo
    pivo_coluna = variavel_entrada
    pivo_linha = pivo_linha(pivo_coluna)
    raise UnboundedProblem unless pivo_linha
    var_sai = var_basica_linha(pivo_linha)
    sub_var_basica(var_sai => pivo_coluna)

    pivo_racional = Rational(1, @retricoes[pivo_linha][pivo_coluna])

    # update pivo row
    @retricoes[pivo_linha] *= pivo_racional
    @constantes[pivo_linha] = pivo_racional * @constantes[pivo_linha]

    # update objective
    @objetiva -= @objetiva[pivo_coluna] * @retricoes[pivo_linha]

    # update A and B
    (indice_linha - [pivo_linha]).each do |row_ix|
      r = @retricoes[row_ix][pivo_coluna]
      @retricoes[row_ix] -= r * @retricoes[pivo_linha]
      @constantes[row_ix] -= r * @constantes[pivo_linha]
    end

    att_solucao
  end

  def sub_var_basica(hash)
    from, to = hash.keys.first, hash.values.first
    @vars_basicas.delete(from)
    @vars_basicas << to
    @vars_basicas.sort!
  end

  def pivo_linha(column_ix)
    row_ix_a_and_b = indice_linha.map { |row_ix|
      [row_ix, @retricoes[row_ix][column_ix], @constantes[row_ix]]
    }.reject { |_, a, b|
      a == 0
    }.reject { |_, a, b|
      (b < 0) ^ (a < 0) # negative sign check
    }
    row_ix, _, _ = *last_min_by(row_ix_a_and_b) { |_, a, b|
      Rational(b, a)
    }
    row_ix
  end

  def var_basica_linha(pivo_linha)
    column_indices.detect do |column_ix|
      @retricoes[pivo_linha][column_ix] == 1 and @vars_basicas.include?(column_ix)
    end
  end

  def indice_linha
    (0...@retricoes.length).to_a
  end

  def column_indices
    (0...@retricoes.first.size).to_a
  end

  def formatted_tableau
    if pode_melhorar?
      pivo_coluna = variavel_entrada
      pivo_linha    = pivo_linha(pivo_coluna)
    else
      pivo_linha = nil
    end
    num_cols = @objetiva.size + 1
    c = formatted_values(@objetiva.to_a)
    b = formatted_values(@constantes.to_a)
    a = @retricoes.to_a.map {|ar| formatted_values(ar.to_a) }
    if pivo_linha
      a[pivo_linha][pivo_coluna] = "*" + a[pivo_linha][pivo_coluna]
    end
    max = (c + b + a + ["1234567"]).flatten.map(&:size).max
    result = []
    result << c.map {|c| c.rjust(max, " ") }
    a.zip(b) do |arow, brow|
      result << (arow + [brow]).map {|a| a.rjust(max, " ") }
      result.last.insert(arow.length, "|")
    end
    lines = result.map {|b| b.join("  ") }
    max_line_length = lines.map(&:length).max
    lines.insert(1, "-"*max_line_length)
    lines.join("\n")
  end

  def formatted_values(array)
    array.map {|c| "%2.3f" % c }
  end

  # like Enumerable#min_by except if multiple values are minimum 
  # it returns the last
  def last_min_by(array)
    best_element, best_value = nil, nil
    array.each do |element|
      value = yield element
      if !best_element || value <= best_value
        best_element, best_value = element, value
      end
    end
    best_element
  end

  def assert(boolean)
    raise unless boolean
  end
end

simplex = Simplex.new([3, 5], [[1, 0], [0, 1], [3, 2]], [4, 6, 18])
simplex.solucao
puts simplex.formatted_tableau
