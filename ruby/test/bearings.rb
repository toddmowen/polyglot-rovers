require 'test/unit'
require_relative '../main/bearings'
include BearingNames

class TestBearing < Test::Unit::TestCase

  def test_to_s
    all_points = [N, S, E, W]
    to_strings = all_points.map(&:to_s)
    assert_equal(['N', 'S', 'E', 'W'], to_strings)
  end

end
