module Example (song) where

song :: String
song = unlines $ map verse [0..7]

verse :: Integer -> String
verse 7 = "I know an old lady who swallowed a horse.\nShe's dead, of course!\n"
verse n = unlines $ first n : map refrain [n, (n-1)..0] 

first :: Integer -> String
first n = "I know an old lady who swallowed a " ++
  case n of
    0 -> "fly."
    1 -> "spider.\nIt wriggled and jiggled and tickled inside her."
    2 -> "bird.\nHow absurd to swallow a bird!"
    3 -> "cat.\nImagine that, to swallow a cat!"
    4 -> "dog.\nWhat a hog, to swallow a dog!"
    5 -> "goat.\nJust opened her throat and swallowed a goat!"
    6 -> "cow.\nI don't know how she swallowed a cow!"
    _ -> error "Not enough things to eat!"

refrain :: Integer -> String
refrain 0 = "I don't know why she swallowed the fly. Perhaps she'll die."
refrain n = "She swallowed the " ++
  case n of
    1 -> "spider to catch the fly."
    2 -> "bird to catch the spider that wriggled and jiggled and tickled inside her."
    3 -> "cat to catch the bird."
    4 -> "dog to catch the cat."
    5 -> "goat to catch the dog."
    6 -> "cow to catch the goat."
    _ -> error "Not enough things to eat!"

lyrics :: String
lyrics =
        "I know an old lady who swallowed a fly.\n\
        \I don't know why she swallowed the fly. Perhaps she'll die.\n\
        \\n\
        \I know an old lady who swallowed a spider.\n\
        \It wriggled and jiggled and tickled inside her.\n\
        \She swallowed the spider to catch the fly.\n\
        \I don't know why she swallowed the fly. Perhaps she'll die.\n\
        \\n\
        \I know an old lady who swallowed a bird.\n\
        \How absurd to swallow a bird!\n\
        \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
        \She swallowed the spider to catch the fly.\n\
        \I don't know why she swallowed the fly. Perhaps she'll die.\n\
        \\n\
        \I know an old lady who swallowed a cat.\n\
        \Imagine that, to swallow a cat!\n\
        \She swallowed the cat to catch the bird.\n\
        \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
        \She swallowed the spider to catch the fly.\n\
        \I don't know why she swallowed the fly. Perhaps she'll die.\n\
        \\n\
        \I know an old lady who swallowed a dog.\n\
        \What a hog, to swallow a dog!\n\
        \She swallowed the dog to catch the cat.\n\
        \She swallowed the cat to catch the bird.\n\
        \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
        \She swallowed the spider to catch the fly.\n\
        \I don't know why she swallowed the fly. Perhaps she'll die.\n\
        \\n\
        \I know an old lady who swallowed a goat.\n\
        \Just opened her throat and swallowed a goat!\n\
        \She swallowed the goat to catch the dog.\n\
        \She swallowed the dog to catch the cat.\n\
        \She swallowed the cat to catch the bird.\n\
        \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
        \She swallowed the spider to catch the fly.\n\
        \I don't know why she swallowed the fly. Perhaps she'll die.\n\
        \\n\
        \I know an old lady who swallowed a cow.\n\
        \I don't know how she swallowed a cow!\n\
        \She swallowed the cow to catch the goat.\n\
        \She swallowed the goat to catch the dog.\n\
        \She swallowed the dog to catch the cat.\n\
        \She swallowed the cat to catch the bird.\n\
        \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
        \She swallowed the spider to catch the fly.\n\
        \I don't know why she swallowed the fly. Perhaps she'll die.\n\
        \\n\
        \I know an old lady who swallowed a horse.\n\
        \She's dead, of course!\n"
    