defmodule Mal do

  @type t ::
    { :string, String.t() } |
    { :symbol, String.t() } |
    { :keyword, String.t() } |
    { :number, integer() } |
    { :boolean, boolean() } |
    { :nil } |
    { :void } |
    { :list, list(t) } |
    { :vector, map() } |
    { :map, map() }

end
