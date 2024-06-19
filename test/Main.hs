module Main where

import CommonAPI
import AuthTest
import AdminAPI

main :: IO ()
main = do
    putStrLn "Test suite not yet  implemented."
    putStrLn  "Test Cases are running"
    -- login_user_test "datasample/userInfo/userlogin.json"
    -- login_admin_test "datasample/adminInfo/adminlogin.json"
    -- register_user_test "datasample/user1.json"
    -- register_admin_test "datasample/admin1.json"
    -- get_facility_test 10
    -- add_facility_test "facility.json"
    -- update_facility_test 12 "facility.json"
    -- delete_facility_test 14
    -- get_facilities_test
    -- get_ratings_test 7
    get_top_facility_test