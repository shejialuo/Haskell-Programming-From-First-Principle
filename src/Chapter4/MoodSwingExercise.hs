module Chapter4.MoodSwingExercise where

data Mood = Blah | Woot deriving (Show, Eq)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah
