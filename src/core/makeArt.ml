module Of(N : Name.S)(D : Data.S) : Art.S with type name = N.t
                                           and type data = D.t =
  Insts.Nominal.MakeArt(N)(D)
