//
//  types.swift
//  step0_repl
//
//  Created by Joe Watson on 2020/06/12.
//  Copyright © 2020 Joe Watson. All rights reserved.
//

public indirect enum Mal : Equatable {
    case int(Int)
    case list([Mal])
}

