
function EnumConstant(key, value) {
  return Object.freeze({key, value})
}

function Enum(mapping) {

  let constants = Object.keys(mapping).reduce(
    (acc, key) => (acc[key] = EnumConstant(key, mapping[key]), acc),
    {})

  let reverseMapping = Object.keys(mapping).reduce(
    (acc, key) => (acc[mapping[key]] = constants[key], acc),
    {})

  constants.fromValue = (value) => reverseMapping[value]

  return Object.freeze(constants)
}
