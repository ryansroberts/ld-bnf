{

  "drug": {
    "expand": true,
    "cwd":  "./src/data/json/drug/",
    "src":  [ "**/*.json" ],
    "dest": "./dist/www/drug/",
    "ext":  ".html"
  },

  "aToZ": {
    "files": [
      {"src": "./src/data/json/aToZ/drug.json", "dest": "./dist/www/drug/index.html"},
      {"src": "./src/data/json/aToZ/drugClass.json", "dest": "./dist/www/drugClass/index.html"},
      {"src": "./src/data/json/aToZ/clinicalMedicinalProductInformation.json", "dest": "./dist/www/clinicalMedicinalProductInformation/index.html"}
    ]
  },

  "drugClass": {
    "expand": true,
    "cwd":  "./src/data/json/drugClass/",
    "src":  [ "**/*.json" ],
    "dest": "./dist/www/drugClass/",
    "ext":  ".html"
  },

  "clinicalMedicinalProductInformation": {
    "expand": true,
    "cwd":  "./src/data/json/clinicalMedicinalProductInformation/",
    "src":  [ "**/*.json" ],
    "dest": "./dist/www/clinicalMedicinalProductInformation/",
    "ext":  ".html"
  },

  "medicinalForm": {
    "expand": true,
    "cwd":  "./src/data/json/medicinalForm/",
    "src":  [ "**/*.json" ],
    "dest": "./dist/www/medicinalForm/",
    "ext":  ".html"
  }

}
