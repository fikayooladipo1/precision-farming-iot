;; Smart Irrigation System Contract
;; Monitor soil conditions, control irrigation systems, and optimize crop yield predictions

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_SENSOR_DATA (err u101))
(define-constant ERR_INVALID_FARM_ID (err u102))
(define-constant ERR_IRRIGATION_SYSTEM_ERROR (err u103))
(define-constant ERR_INSUFFICIENT_WATER (err u104))
(define-constant ERR_SENSOR_NOT_FOUND (err u105))
(define-constant ERR_FARM_NOT_FOUND (err u106))

;; Data Variables
(define-data-var next-farm-id uint u1)
(define-data-var next-sensor-id uint u1)
(define-data-var total-water-used uint u0)
(define-data-var emergency-threshold uint u20) ;; Critical moisture level

;; Data Maps
;; Farm registration and management
(define-map farms
    { farm-id: uint }
    {
        owner: principal,
        name: (string-ascii 100),
        location: (string-ascii 200),
        crop-type: (string-ascii 50),
        area-size: uint, ;; in square meters
        registration-date: uint,
        is-active: bool
    }
)

;; Soil sensors data
(define-map soil-sensors
    { sensor-id: uint }
    {
        farm-id: uint,
        sensor-type: (string-ascii 50),
        location-x: uint,
        location-y: uint,
        last-reading: uint,
        is-operational: bool
    }
)

;; Soil condition readings
(define-map soil-conditions
    { farm-id: uint, timestamp: uint }
    {
        moisture-level: uint, ;; percentage 0-100
        ph-level: uint, ;; pH * 10 (e.g., 65 = pH 6.5)
        temperature: uint, ;; Celsius * 10
        nitrogen-level: uint, ;; ppm
        phosphorus-level: uint, ;; ppm
        potassium-level: uint, ;; ppm
        sensor-id: uint
    }
)

;; Irrigation events
(define-map irrigation-events
    { farm-id: uint, event-id: uint }
    {
        timestamp: uint,
        water-amount: uint, ;; liters
        duration: uint, ;; minutes
        trigger-type: (string-ascii 20), ;; "manual", "automatic", "emergency"
        moisture-before: uint,
        moisture-after: uint
    }
)

;; Crop yield predictions
(define-map yield-predictions
    { farm-id: uint, prediction-date: uint }
    {
        predicted-yield: uint, ;; kg per hectare
        confidence-level: uint, ;; percentage 0-100
        harvest-date: uint,
        factors-considered: (list 10 (string-ascii 30))
    }
)

;; Water usage tracking
(define-map water-usage-daily
    { farm-id: uint, date: uint }
    {
        total-usage: uint, ;; liters
        irrigation-count: uint,
        efficiency-score: uint ;; 0-100
    }
)

;; Farm authorization for operators
(define-map farm-operators
    { farm-id: uint, operator: principal }
    { authorized: bool, role: (string-ascii 20) }
)

;; Private Functions

;; Check if caller is authorized to manage farm
(define-private (is-farm-authorized (farm-id uint) (caller principal))
    (match (map-get? farms { farm-id: farm-id })
        farm-data 
            (or 
                (is-eq caller (get owner farm-data))
                (default-to false (get authorized (map-get? farm-operators { farm-id: farm-id, operator: caller })))
            )
        false
    )
)

;; Calculate irrigation recommendation based on soil conditions
(define-private (calculate-irrigation-need (moisture uint) (temperature uint) (crop-type (string-ascii 50)))
    (let (
        (base-need (if (<= moisture u30) u100
                  (if (<= moisture u50) u60
                  (if (<= moisture u70) u30 u0))))
        (temp-adjustment (if (>= temperature u300) u20 u0))
        (crop-multiplier (if (is-eq crop-type "rice") u150
                        (if (is-eq crop-type "wheat") u100
                        (if (is-eq crop-type "corn") u120 u100))))
    )
        (/ (* (+ base-need temp-adjustment) crop-multiplier) u100)
    )
)

;; Validate sensor reading values
(define-private (is-valid-sensor-reading (moisture uint) (ph uint) (temp uint))
    (and 
        (<= moisture u100)
        (and (>= ph u40) (<= ph u90)) ;; pH 4.0 to 9.0
        (and (>= temp u0) (<= temp u500)) ;; 0C to 50C
    )
)

;; Public Functions

;; Register a new farm
(define-public (register-farm 
    (name (string-ascii 100)) 
    (location (string-ascii 200)) 
    (crop-type (string-ascii 50))
    (area-size uint))
    (let (
        (farm-id (var-get next-farm-id))
        (current-time block-height)
    )
        (map-set farms 
            { farm-id: farm-id }
            {
                owner: tx-sender,
                name: name,
                location: location,
                crop-type: crop-type,
                area-size: area-size,
                registration-date: current-time,
                is-active: true
            }
        )
        (var-set next-farm-id (+ farm-id u1))
        (ok farm-id)
    )
)

;; Register a soil sensor
(define-public (register-sensor 
    (farm-id uint)
    (sensor-type (string-ascii 50))
    (location-x uint)
    (location-y uint))
    (let (
        (sensor-id (var-get next-sensor-id))
        (current-time block-height)
    )
        (asserts! (is-some (map-get? farms { farm-id: farm-id })) ERR_INVALID_FARM_ID)
        (asserts! (is-farm-authorized farm-id tx-sender) ERR_NOT_AUTHORIZED)
        
        (map-set soil-sensors
            { sensor-id: sensor-id }
            {
                farm-id: farm-id,
                sensor-type: sensor-type,
                location-x: location-x,
                location-y: location-y,
                last-reading: current-time,
                is-operational: true
            }
        )
        (var-set next-sensor-id (+ sensor-id u1))
        (ok sensor-id)
    )
)

;; Record soil condition data
(define-public (record-soil-conditions
    (farm-id uint)
    (moisture-level uint)
    (ph-level uint)
    (temperature uint)
    (nitrogen-level uint)
    (phosphorus-level uint)
    (potassium-level uint)
    (sensor-id uint))
    (let (
        (current-time block-height)
    )
        (asserts! (is-some (map-get? farms { farm-id: farm-id })) ERR_INVALID_FARM_ID)
        (asserts! (is-some (map-get? soil-sensors { sensor-id: sensor-id })) ERR_SENSOR_NOT_FOUND)
        (asserts! (is-valid-sensor-reading moisture-level ph-level temperature) ERR_INVALID_SENSOR_DATA)
        
        (map-set soil-conditions
            { farm-id: farm-id, timestamp: current-time }
            {
                moisture-level: moisture-level,
                ph-level: ph-level,
                temperature: temperature,
                nitrogen-level: nitrogen-level,
                phosphorus-level: phosphorus-level,
                potassium-level: potassium-level,
                sensor-id: sensor-id
            }
        )
        
        ;; Update sensor last reading time
        (match (map-get? soil-sensors { sensor-id: sensor-id })
            sensor-data (map-set soil-sensors
                { sensor-id: sensor-id }
                (merge sensor-data { last-reading: current-time })
            )
            false
        )
        
        (ok true)
    )
)

;; Trigger irrigation system
(define-public (trigger-irrigation
    (farm-id uint)
    (water-amount uint)
    (duration uint)
    (trigger-type (string-ascii 20)))
    (let (
        (current-time block-height)
        (event-id (+ (default-to u0 (get irrigation-count (map-get? water-usage-daily { farm-id: farm-id, date: current-time }))) u1))
    )
        (asserts! (is-some (map-get? farms { farm-id: farm-id })) ERR_INVALID_FARM_ID)
        (asserts! (is-farm-authorized farm-id tx-sender) ERR_NOT_AUTHORIZED)
        (asserts! (> water-amount u0) ERR_INSUFFICIENT_WATER)
        
        (map-set irrigation-events
            { farm-id: farm-id, event-id: event-id }
            {
                timestamp: current-time,
                water-amount: water-amount,
                duration: duration,
                trigger-type: trigger-type,
                moisture-before: u0, ;; To be updated with actual reading
                moisture-after: u0   ;; To be updated with actual reading
            }
        )
        
        ;; Update daily water usage
        (match (map-get? water-usage-daily { farm-id: farm-id, date: current-time })
            usage-data (map-set water-usage-daily
                { farm-id: farm-id, date: current-time }
                {
                    total-usage: (+ (get total-usage usage-data) water-amount),
                    irrigation-count: (+ (get irrigation-count usage-data) u1),
                    efficiency-score: (get efficiency-score usage-data)
                }
            )
            (map-set water-usage-daily
                { farm-id: farm-id, date: current-time }
                {
                    total-usage: water-amount,
                    irrigation-count: u1,
                    efficiency-score: u75 ;; Default efficiency score
                }
            )
        )
        
        (var-set total-water-used (+ (var-get total-water-used) water-amount))
        (ok event-id)
    )
)

;; Generate yield prediction
(define-public (generate-yield-prediction
    (farm-id uint)
    (predicted-yield uint)
    (confidence-level uint)
    (harvest-date uint)
    (factors (list 10 (string-ascii 30))))
    (let (
        (current-time block-height)
    )
        (asserts! (is-some (map-get? farms { farm-id: farm-id })) ERR_INVALID_FARM_ID)
        (asserts! (is-farm-authorized farm-id tx-sender) ERR_NOT_AUTHORIZED)
        (asserts! (<= confidence-level u100) ERR_INVALID_SENSOR_DATA)
        
        (map-set yield-predictions
            { farm-id: farm-id, prediction-date: current-time }
            {
                predicted-yield: predicted-yield,
                confidence-level: confidence-level,
                harvest-date: harvest-date,
                factors-considered: factors
            }
        )
        (ok true)
    )
)

;; Authorize farm operator
(define-public (authorize-operator (farm-id uint) (operator principal) (role (string-ascii 20)))
    (begin
        (asserts! (is-some (map-get? farms { farm-id: farm-id })) ERR_INVALID_FARM_ID)
        (asserts! (is-farm-authorized farm-id tx-sender) ERR_NOT_AUTHORIZED)
        
        (map-set farm-operators
            { farm-id: farm-id, operator: operator }
            { authorized: true, role: role }
        )
        (ok true)
    )
)

;; Read-only functions

;; Get farm information
(define-read-only (get-farm-info (farm-id uint))
    (map-get? farms { farm-id: farm-id })
)

;; Get latest soil conditions
(define-read-only (get-latest-soil-conditions (farm-id uint))
    (let (
        (current-time block-height)
    )
        (map-get? soil-conditions { farm-id: farm-id, timestamp: current-time })
    )
)

;; Get water usage for a specific date
(define-read-only (get-water-usage (farm-id uint) (date uint))
    (map-get? water-usage-daily { farm-id: farm-id, date: date })
)

;; Get total water used across all farms
(define-read-only (get-total-water-usage)
    (var-get total-water-used)
)

;; Get yield prediction
(define-read-only (get-yield-prediction (farm-id uint) (prediction-date uint))
    (map-get? yield-predictions { farm-id: farm-id, prediction-date: prediction-date })
)

;; Get sensor information
(define-read-only (get-sensor-info (sensor-id uint))
    (map-get? soil-sensors { sensor-id: sensor-id })
)

;; Check if emergency irrigation is needed
(define-read-only (check-emergency-status (farm-id uint))
    (let (
        (current-time block-height)
    )
        (match (map-get? soil-conditions { farm-id: farm-id, timestamp: current-time })
            conditions (< (get moisture-level conditions) (var-get emergency-threshold))
            false
        )
    )
)

