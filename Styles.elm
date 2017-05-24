module Styles exposing(..)

type ButtonColor = Green | Red | Blue

buttonStyles: List (String, String)
buttonStyles =
    [ ("background-color", "#4CAF50")
    , ("color", "white")
    , ("margin-top", "20px")
    , ("padding", "7px")
    , ("border","none")
    , ("border-radius","5px")
    , ("margin-right", "20px")
    , ("float", "right")
    , ("cursor", "pointer")
    ]

buttonSmallStyles: ButtonColor -> List (String, String)
buttonSmallStyles buttonColor =
    let buttonColorCode =
        case buttonColor of
             Green -> "#4CAF50"
             Red -> "#FFAB91"
             Blue -> "#81D4FA"
    in
    [ ("background-color", buttonColorCode)
    , ("color", "white")
    , ("border","none")
    , ("border-radius","5px")
    , ("cursor", "pointer")
    ]

marginButtonStyles: List (String, String)
marginButtonStyles =
    [ ("margin-top", "20px")
    , ("margin-bottom", "20px")
    , ("margin-left", "30px")
    , ("padding", "7px")
    , ("float", "left")
    , ("background-color", "#4CAF50")
    , ("color", "white")
    , ("border","none")
    , ("border-radius","5px")
    , ("cursor", "pointer")
    ]


buttonNeutralStyles: List (String, String)
buttonNeutralStyles =
    [ ("margin-top", "20px")
    , ("margin-bottom", "20px")
    , ("padding", "5px")
    , ("border","none")
    , ("border-radius","5px")
    , ("margin-right", "10px")
    , ("float", "right")
    , ("cursor", "pointer")
    ]

containerStyles: List (String, String)
containerStyles =
    [ ("border", "2px solid #A5D6A7")
    , ("margin-top", "10px")
    , ("margin-left", "20px")
    , ("padding", "10px")
    , ("float", "left")
    , ("border-radius","7px")
    ]


container2Styles: List (String, String)
container2Styles =
    [ ("border", "2px solid #4CAF50")
    , ("margin-top", "5px")
    , ("margin-left", "10px")
    , ("padding", "10px")
    , ("float", "left")
    , ("border-radius","7px")
    ]

containerNeutralStyles: List (String, String)
containerNeutralStyles =
    [ ("width", "100%")
    , ("float", "left")
    ]

containerNeutral2Styles: List (String, String)
containerNeutral2Styles =
    [ ("padding", "10px")
    , ("float", "left")
    ]


tableStyles : List (String, String)
tableStyles =
    [ ("border", "1px solid black")
    , ("margin", "0 auto")
    , ("border-collapse", "collapse")
    ]


unitStyles : List (String, String)
unitStyles =
    [ ("border","1px solid black")
    , ("padding", "5px")
    ]

tableHeaderStyles: List (String, String)
tableHeaderStyles = [("width", "100px")]

tableUnitStyles: List (String, String)
tableUnitStyles =
    [ ("width", "200px")
    ]

inlineStyles : List (String, String)
inlineStyles =
    [ ("float", "left")
    , ("margin-right", "10px")
    ]

headerStyles : List (String, String)
headerStyles =
    [ ("font-weight", "bold")
    , ("margin-top", "5px")
    , ("margin-bottom", "10px")
    ]
