module Main where

import CommonAPI
import AuthTest
import AdminAPI
import UserAPI


testCaseAuthTest :: IO ()
testCaseAuthTest = do
    putStrLn "\nTestCase for Auth"

    putStrLn "\ntest cases 1) Testing register_user"
    register_users_test

    putStrLn "\ntest cases 2) Testing register_admin"
    register_admins_test

    putStrLn "\ntest cases 3) Testing login_user"
    login_user_test "datasample/userInfo/userlogin.json"

    putStrLn "\ntest cases 4) Testing login_admin"
    login_admin_test "datasample/adminInfo/adminlogin.json"

    where
        register_users_test :: IO ()
        register_users_test = do
            register_user_test "datasample/userInfo/user1.json"
            register_user_test "datasample/userInfo/user2.json"
            register_user_test "datasample/userInfo/user3.json"
            register_user_test "datasample/userInfo/user4.json"
            register_user_test "datasample/userInfo/user5.json"

        register_admins_test :: IO ()
        register_admins_test = do
            register_admin_test "datasample/adminInfo/admin1.json"
            register_admin_test "datasample/adminInfo/admin2.json"
            register_admin_test "datasample/adminInfo/admin3.json"
            register_admin_test "datasample/adminInfo/admin4.json"
            register_admin_test "datasample/adminInfo/admin5.json"

testCaseAdminAPI :: IO ()
testCaseAdminAPI = do
    putStrLn "\nTestCase for Admin API's"

    putStrLn "\ntest cases 1) Testing add_facility"
    add_facilities_test

    putStrLn "\ntest cases 2) Testing update_facility"
    update_facility_test 6 "datasample/facilityInfo/facility6update.json" -- price updated

    putStrLn "\ntest cases 3) Testing delete_facility"
    delete_facility_test 9 

    putStrLn "\ntest cases 4) Testing create_group"
    create_groups_test 

    putStrLn "\ntest cases 5) Testing update_facility_group"
    update_facility_group_test 1 1
    update_facility_group_test 6 1
    update_facility_group_test 7 1

    putStrLn "\ntest cases 6) Testing remove_facility_group"
    remove_facility_group_test 7

    putStrLn "\ntest cases 7) Testing delete_group"
    delete_group_test 2

    putStrLn "\ntest cases 8) Testing set_holiday"
    set_holiday_test "datasample/facilityInfo/facilityStatus1.json"
    set_holiday_test "datasample/facilityInfo/facilityStatus2.json"

    -- putStrLn "\ntest cases 9) Testing set_holiday_group"
    -- set_holiday_group_test 1 "datasample/groupInfo/groupHoliday.json"

    putStrLn "\ntest cases 10) Testing delete_holiday"
    delete_holiday_test 2

    -- putStrLn "\ntest cases 11) Testing update_grouped_facilities"
    -- update_grouped_facilities_test 1 "datasample/facilityInfo/facilityUpdateByGroup.json"


    where
        add_facilities_test :: IO ()
        add_facilities_test = do
            add_facility_test "datasample/facilityInfo/facility1.json"
            add_facility_test "datasample/facilityInfo/facility2.json" 
            add_facility_test "datasample/facilityInfo/facility3.json" 
            add_facility_test "datasample/facilityInfo/facility4.json" 
            add_facility_test "datasample/facilityInfo/facility5.json" 
            add_facility_test "datasample/facilityInfo/facility6.json" 
            add_facility_test "datasample/facilityInfo/facility7.json" 
            add_facility_test "datasample/facilityInfo/facility8.json" 
            add_facility_test "datasample/facilityInfo/facility9.json" 
            add_facility_test "datasample/facilityInfo/facility10.json" 

        create_groups_test :: IO ()
        create_groups_test = do
            create_group_test "datasample/groupInfo/group1.json"
            create_group_test "datasample/groupInfo/group2.json"
            create_group_test "datasample/groupInfo/group3.json"





testCaseCommonAPI :: IO ()
testCaseCommonAPI = do
    putStrLn "\nTestCase for Common API's"

    putStrLn "\ntest case 1) Testing get_facilities"
    get_facilities_test

    putStrLn "\ntest case 2) Testing get_facility"
    get_facility_test 1
    get_facility_test 2
    
    putStrLn "\ntest case 3) Testing get_ratings"
    get_ratings_test 1
    get_ratings_test 2

    putStrLn "\ntest case 4) Testing get_top_facility_test"
    get_top_facility_test

main :: IO ()
main = do
    putStrLn  "Test Cases are running"
    -- testcaseAuthTest
    -- testCaseCommonAPI
    testCaseAdminAPI


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
    -- get_top_facility_test
    -- get_bookings_test
    -- get_booking_status_test 1
    -- search_available_slots_test 9