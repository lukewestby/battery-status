var _lukewestby$battery_status$Native_BatteryStatus = (function () {
  function getBattery() {
    return window.navigator.getBattery()
  }

  function isSupported() {
    return typeof window.navigator.getBattery === 'function'
  }

  function getProperty(property) {
    return function () {
      return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
        if (!isSupported()) {
          callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'Unsupported' }))
        } else {
          getBattery().then(function (battery) {
            callback(_elm_lang$core$Native_Scheduler.succeed(battery[property]))
          })
        }
      })
    }
  }

  function on(eventName, decoder, toTask) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
      if (!isSupported()) {
        return function () { }
      }

      function performTask(event) {
          _elm_lang$core$Native_Scheduler.rawSpawn(toTask(event));
      }

      getBattery().then(function (battery) {
        battery.addEventListener(eventName, performTask)
      })

  		return function () {
        getBattery().then(function (battery) {
          battery.removeEventListener(eventName, performTask)
        })
  		}
  	})
  }

  return {
    isSupported: isSupported(),
    isCharging: getProperty('charging'),
    chargeLevel: getProperty('level'),
    chargingTime: getProperty('chargingTime'),
    dischargingTime: getProperty('dischargingTime'),
    on: on
  }
}())
