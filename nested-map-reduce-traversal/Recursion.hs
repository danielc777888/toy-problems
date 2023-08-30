-- https://github.com/josevalim/nested-map-reduce-traversal
-- solution using recursion

module Recursion where

data Section = Section { title :: String,
                         resetLessonPosition :: Bool,
                         position :: Int,
                         lessons :: [Lesson] } deriving Show

data Lesson = Lesson { name :: String,
                       lessonPosition :: Int } deriving Show

sections =  [Section {
               title = "Getting started",
               resetLessonPosition = False,
               position = 0,
               lessons = [
                   Lesson { name = "Welcome", lessonPosition = 0 },
                   Lesson { name = "Installation", lessonPosition = 0 }
               ]
             },
             Section {
               title = "Basic operator",
               resetLessonPosition = False,
               position = 0,
               lessons = [
                   Lesson { name = "Addition / Subtraction", lessonPosition = 0 },
                   Lesson { name = "Multiplication / Division", lessonPosition = 0 }
               ]
             },
             Section {
               title = "Advanced topics",
               resetLessonPosition = True,
               position = 0,
               lessons = [
                   Lesson { name = "Mutability", lessonPosition =  0 },
                   Lesson { name = "Immutability", lessonPosition =  0 }
               ]
             }]

updateSections :: [Section] -> Int -> Int -> [Section]
updateSections [] _ _ = []
updateSections (s:ss) sc lc =
  s { position = sc', lessons = ls }: updateSections ss sc' (lessonPosition (last ls))
  where sc' = sc + 1
        ls = updateLessons (lessons s) (if resetLessonPosition s == True then 0 else lc)

updateLessons :: [Lesson] -> Int -> [Lesson]
updateLessons [] _ = []
updateLessons (l:ls) lc = l { lessonPosition = lc' }: updateLessons ls lc'
  where lc' = lc + 1

solve :: IO ()
solve = print $ updateSections sections 0 0
