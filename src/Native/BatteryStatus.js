var _lukewestby$battery_status$Native_BatteryStatus = (function () {
  var scheduler = _elm_lang$core$Native_Scheduler

  function getBattery() {
    return window.navigator.getBattery()
  }

  function isSupported() {
    return typeof window.navigator.getBattery === 'function'
  }

  function toBatteryStatus(battery) {
    return {
      isCharging: battery.charging,
      level: battery.level,
      chargingTime: battery.chargingTime * 1000,
      dischargingTime: battery.dischargingTime * 1000
    }
  }

  var batteryStatus = scheduler.nativeBinding(function (callback) {
    if (!isSupported()) {
      callback(scheduler.fail({ ctor: 'Unsupported' }))
    } else {
      getBattery().then(function (battery) {
        callback(scheduler.succeed(toBatteryStatus(battery)))
      })
    }
  })

  var eventNames = [
    'chargingchange',
    'levelchange',
    'chargingtimechange',
    'dischargingtimechange'
  ]

  function onChange(toTask) {
    return scheduler.nativeBinding(function (callback) {
      if (!isSupported()) {
        return
      }

      function performTask(event) {
        scheduler.rawSpawn(toTask(toBatteryStatus(event.target)))
      }

      getBattery().then(function (battery) {
        eventNames.forEach(function (eventName) {
          battery.addEventListener(eventName, performTask)
        })
      })

  		return function () {
        getBattery().then(function (battery) {
          eventNames.forEach(function (eventName) {
            battery.removeEventListener(eventName, performTask)
          })
        })
  		}
  	})
  }

  return {
    isSupported: isSupported(),
    batteryStatus: batteryStatus,
    onChange: onChange
  }
}())
