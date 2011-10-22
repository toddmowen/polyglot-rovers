require 'test/unit'
require_relative '../main/rovers'


class TestRover < Test::Unit::TestCase

  include Bearings  # for test_dsl

  def setup
    @p = Plateau.new(6,8)
    @r = Rover.new(4,7,Vector[1,0],@p)
  end

  def test_equals
    assert_equal(@r, Rover.new(4,7,Vector[1,0]))
    assert_not_equal(@r, Rover.new(3,7,Vector[1,0]))
    assert_not_equal(@r, Rover.new(4,7,Vector[-1,0]))
  end

  def test_initial_position_out_of_bounds
    assert_raise(OutOfBounds) do
      Rover.new(4,9,Vector[1,0],@p)
    end
  end

  def test_dsl
    assert_equal(Rover.new(4,7,E), @r)
  end

  def test_M
    @r.M
    assert_equal(Rover.new(5,7,E), @r)
    @r.M
    assert_equal(Rover.new(6,7,E), @r)

    assert_raise(OutOfBounds) do
      @r.M
    end
  end

  def test_R
    @r.R
    assert_equal(Rover.new(4,7,S), @r)
    @r.R
    assert_equal(Rover.new(4,7,W), @r)
    @r.R
    assert_equal(Rover.new(4,7,N), @r)
    @r.R
    assert_equal(Rover.new(4,7,E), @r)
  end

  def test_L
    @r.L
    assert_equal(Rover.new(4,7,N), @r)
    @r.L
    assert_equal(Rover.new(4,7,W), @r)
    @r.L
    assert_equal(Rover.new(4,7,S), @r)
    @r.L
    assert_equal(Rover.new(4,7,E), @r)
  end

  def test_execute
    @r.execute(:M, :M, :L, :M, :L, :L, :M, :R, :M)
    assert_equal(Rover.new(5,7,W), @r)
  end

end
