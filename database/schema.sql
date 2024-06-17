-- Booking Status type for booking relation
CREATE TYPE booking_st AS ENUM ('booked', 'canclled', 'activate');

-- Facility Status type for facility_status relation
CREATE TYPE facility_st AS ENUM ('maintenance', 'holiday', 'bookedforSubscriber');

-- Day of weeks type for subscription relation
CREATE TYPE day_of_week AS ENUM ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');

-- Admin Relation
CREATE TABLE admins (
    admin_id        SERIAL PRIMARY KEY,
    admin_name      VARCHAR(200) NOT NULL,
    admin_email     VARCHAR(200) NOT NULL UNIQUE,
    admin_pass      VARCHAR(50)  NOT NULL,  -- later chkpass type will use
    phone_number    VARCHAR(20),
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ
);


-- Create the trigger function and trigger for update_on 'admins' Relation
CREATE OR REPLACE FUNCTION update_admins_timestamp() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_on = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER before_admins_update
BEFORE UPDATE ON admins
FOR EACH ROW
EXECUTE FUNCTION update_admins_timestamp();


-- User Relation
CREATE TABLE users (
    user_id         SERIAL PRIMARY KEY,
    user_name       VARCHAR(200) NOT NULL,
    user_email      VARCHAR(200) NOT NULL UNIQUE,
    user_pass       VARCHAR(50) NOT NULL,  -- later chkpass type will use
    phone_number    VARCHAR(20),
    user_address    TEXT,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ
);


-- Create the trigger function and trigger for update_on 'users' Relation
CREATE OR REPLACE FUNCTION update_users_timestamp() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_on = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER before_users_update
BEFORE UPDATE ON users
FOR EACH ROW
EXECUTE FUNCTION update_users_timestamp();


-- Group Relation to manage facilitys easier
CREATE TABLE groups (
    group_id        SERIAL PRIMARY KEY,
    group_name      VARCHAR(200) NOT NULL,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ,
    admin_id        INT REFERENCES admins(admin_id) NOT NULL
);


-- Create the trigger function and trigger for update_on 'gropus' Relation
CREATE OR REPLACE FUNCTION update_groups_timestamp() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_on = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER before_groups_update
BEFORE UPDATE ON groups
FOR EACH ROW
EXECUTE FUNCTION update_groups_timestamp();


-- Facility Relation 
CREATE TABLE facility (
    facility_id     SERIAL PRIMARY KEY,
    facility_name   VARCHAR(200) NOT NULL,
    facility_sport  VARCHAR(50) NOT NULL,
    price_per_slot  INT NOT NULL,
    slot_duration   INT NOT NULL,
    open_time       TIME NOT NULL,
    close_time      TIME NOT NULL,
    -- facility_image blob type later will use
    facility_address TEXT NOT NULL,
    city            VARCHAR(50) NOT NULL,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ,
    group_id        INT REFERENCES groups(group_id)
);

-- Create the trigger function and trigger for update_on 'facility' Relation
CREATE OR REPLACE FUNCTION update_facility_timestamp() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_on = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER before_facility_update
BEFORE UPDATE ON facility
FOR EACH ROW
EXECUTE FUNCTION update_facility_timestamp();


-- Facility Slot Realtion
CREATE TABLE facility_slots (
    slot_id   SERIAL PRIMARY KEY,
    slot_start_time TIME NOT NULL,
    slot_end_time   TIME NOT NULL,
    facility_id     INT REFERENCES facility(facility_id) NOT NULL
);

-- Function for generating facility_slots
CREATE OR REPLACE FUNCTION generate_facility_slots() RETURNS TRIGGER AS $$
DECLARE
    start_time TIME;
    end_time TIME;
    slot_duration INTERVAL;
BEGIN
    start_time := NEW.open_time;
    slot_duration := (NEW.slot_duration || ' hour')::INTERVAL;

    -- Loop to generate slots until the close time
    LOOP
        end_time := start_time + slot_duration;

        -- Check if the end time exceeds the close time
        IF end_time > NEW.close_time THEN
            end_time := NEW.close_time;
        END IF;

        INSERT INTO facility_slots (facility_id, slot_start_time, slot_end_time)
        VALUES (NEW.facility_id, start_time, end_time);

        start_time := end_time;

        -- Exit loop when start_time reaches or exceeds close_time
        EXIT WHEN start_time >= NEW.close_time;
    END LOOP;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


-- Trigger is created for generate facility_slots for facility
CREATE TRIGGER generate_slots_trigger
AFTER INSERT ON facility
FOR EACH ROW
EXECUTE FUNCTION generate_facility_slots();


-- Booking Relation to book facilitys by User for perticular time slot
CREATE TABLE bookings (
    booking_id      SERIAL PRIMARY KEY,
    price           INT NOT NULL,
    booking_status  booking_st NOT NULL,
    booking_token   VARCHAR(200) NOT NULL,
    booking_date    DATE NOT NULL,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ,
    user_id         INT REFERENCES users(user_id) NOT NULL,
    slot_id         INT REFERENCES facility_slots(slot_id) NOT NULL    
);


-- Create the trigger function and trigger for update_on 'bookings' Relation
CREATE OR REPLACE FUNCTION update_bookings_timestamp() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_on = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER before_bookings_update
BEFORE UPDATE ON bookings
FOR EACH ROW
EXECUTE FUNCTION update_bookings_timestamp();


-- Wating List Relation
CREATE TABLE waitinglist (
    watinglist_id   SERIAL PRIMARY KEY,
    price           INT NOT NULL,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ,
    user_id         INT REFERENCES users(user_id) NOT NULL,
    slot_id         INT REFERENCES facility_slots(slot_id) NOT NULL    
);


-- Create the trigger function and trigger for update_on 'waitinglist' Relation
CREATE OR REPLACE FUNCTION update_waitinglist_timestamp() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_on = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER before_waitinglist_update
BEFORE UPDATE ON waitinglist
FOR EACH ROW
EXECUTE FUNCTION update_waitinglist_timestamp();


-- Facility Status Relation for Holiday, Mantenance or Booked Day for Subscriber
CREATE TABLE facility_status (
    status_id       SERIAL PRIMARY KEY,
    status          facility_st NOT NULL,
    start_date      TIMESTAMPTZ NOT NULL,
    end_date        TIMESTAMPTZ NOT NULL,
    facility_id     INT REFERENCES facility(facility_id) NOT NULL
);

-- Rating Relation for Facilitys
CREATE TABLE ratings (
    rating_id       SERIAL PRIMARY KEY,
    rating          NUMERIC(3,2) NOT NULL,
    comment         TEXT,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ,
    user_id         INT REFERENCES users(user_id) NOT NULL,
    facility_id     INT REFERENCES facility(facility_id) NOT NULL
);


-- Create the trigger function and trigger for update_on 'ratings' Relation
CREATE OR REPLACE FUNCTION update_ratings_timestamp() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_on = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER before_ratings_update
BEFORE UPDATE ON ratings
FOR EACH ROW
EXECUTE FUNCTION update_ratings_timestamp();


-- Subscriptions Relation for Recurring Days
-- Assumption it's monthly subscription
-- for hole day in each week.
CREATE TABLE subscriptions (
    subscription_id SERIAL PRIMARY KEY,
    recurring_day   day_of_week NOT NULL,
    price           INT NOT NULL,
    start_on        TIMESTAMPTZ DEFAULT NOW(),
    end_on          TIMESTAMPTZ, -- automatically generated start_on + 28 days
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ,
    user_id         INT REFERENCES users(user_id) NOT NULL,
    facility_id     INT REFERENCES facility(facility_id) NOT NULL
);


-- Create the trigger function and trigger for update_on 'subscriptions' Relation
CREATE OR REPLACE FUNCTION update_subscriptions_timestamp() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_on = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER before_subscriptions_update
BEFORE UPDATE ON subscriptions
FOR EACH ROW
EXECUTE FUNCTION update_subscriptions_timestamp();


-- Create a function for calculate end date
CREATE OR REPLACE FUNCTION calculate_end_date()
RETURNS TRIGGER AS $$
BEGIN
    NEW.end_on := NEW.start_on + INTERVAL '28 days'; -- Calculate end date as 28 days after start date
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create a trigger to call the function before inserting a new row
CREATE TRIGGER before_insert_subscription
BEFORE INSERT ON subscriptions
FOR EACH ROW EXECUTE FUNCTION calculate_end_date();
