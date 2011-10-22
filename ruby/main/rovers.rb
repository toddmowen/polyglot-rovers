require 'matrix'


class Plateau
  def initialize(xbound, ybound)
    @xbound = xbound
    @ybound = ybound
  end

  def in_bounds(vec)
    vec[0].between?(0, @xbound) && vec[1].between?(0, @ybound)
  end
end


class OutOfBounds < Exception
  def to_s
    "Out of bounds (rover fell off the plateau)"
  end
end


class Rover

  attr_reader :plateau, :p, :v

  def initialize(x,y,bearing,plateau=nil)
    @p = Vector[x,y]
    @v = bearing
    @plateau = plateau

    if(@plateau)
      raise OutOfBounds unless @plateau.in_bounds(@p)
    end
  end

  def ==(obj)
    (obj.class == Rover) && (obj.p == @p) && (obj.v == @v)
  end

  # expects a series of symbols
  def execute(*messages)
    messages.each { |msg| self.send(msg) }
  end

  def M
    if @plateau.in_bounds(p_ = @p + @v)
      @p = p_
    else
      # leave @p unchanged, i.e. rover remains in original position
      raise OutOfBounds
    end
  end

  def R
    @v = Matrix[[0,1],[-1,0]] * @v
  end

  def L
    @v = Matrix[[0,-1],[1,0]] * @v
  end

end


module Bearings
  # These objects are immutable (because Vectors are immutable)
  N = Vector[0,1]
  E = Vector[1,0]
  S = Vector[0,-1]
  W = Vector[-1,0]
end