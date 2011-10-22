require 'matrix'


class Bearing < Vector

  # Bearings will be displayed using their name from the BearingNames module,
  # or as a fall-back the same format used by Vector, e.g. "Bearing[1, 1]".
  def to_s
    matching_const = BearingNames.constants.select{|c| BearingNames.const_get(c) == self}[0]
    if (matching_const)
      return matching_const.id2name
    else
      return super.to_s.sub("Vector", "Bearing")
    end
  end

  alias :inspect :to_s
end


module BearingNames
  # These objects are immutable (because Vectors are immutable)
  N = Bearing[0,1]
  E = Bearing[1,0]
  S = Bearing[0,-1]
  W = Bearing[-1,0]
end