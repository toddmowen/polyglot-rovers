require 'test/unit'
require_relative '../main/rovers'


class TestPlateau < Test::Unit::TestCase

  def setup
    @p = Plateau.new(3,5)
  end

  def test_bounds
    assert(@p.in_bounds(0,0))
    assert(@p.in_bounds(3,5))
    assert(@p.in_bounds(3,3))
    assert(!@p.in_bounds(4,3))
    assert(!@p.in_bounds(5,3))
  end

end
