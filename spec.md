validate schema-names data

FHIR schema is an algorythm to implement FHIR Validation.
Each schema consists of set of keywords or rules.
There are 3 types of rules:
* Special type - like elements, resourceType, type. These rules are hard coded into altorythm
* Collection rules - min, max, slicing. These rules are evaluated on top of collection
* Value rules - required, excluded. These rules are evaluated for each value

Schemas may refer each other by URL or type name.
Implementation should provide be two functions `resolve-schema` and `resolve-type`.


There are implicit rules for:
* resourceType, meta.profile, type and extensions - dynamically resolved schemas
* primitive extensions represented as `_<element-name>`
* contained resources (type=Resource)
* Bundle validation (type=Resource)

Schemas are evaluated together in a cooperative manner, if non
of schema recognizes element, `unknown-element` error is added (unless open-world mode is turned on).

## Algorithm

1. Resolve schema-names, if not resolved add to errors schemas which are not resolved
2. Validate schema rules in groups.
   1. Get rules which should be evaluated, from all schemas
   1. Evaluate rules
3. If value is complex
   1. iterate key / values
   2. get schemas for specific key, if no schemas - add element unknown error
   2. handle primitive extensions
   3. if schemas have a type attribute - enrich element schema collection with resolved types
   3. check that value should be array - search in element schemas, that it should be array and check
   4. For array-level rules (like slices) - eval on array level
   4. recursive call validate on element value with updated data-path (in case of array on each value with index in data-path)

## Rules

* resourceType  - type of resource
* baseDefintion - refers base definition
* elements      - map of elements
* required      - collection of required elements
* excluded      - collection of excluded elements
* type          - element type
* profile - set of profiles on element
* refers - resourceType and profile for reference
* binding - valueset biding
* array - is value array or not
* min/max - min/max count in array
* contentReference - recursive reference
* pattern (fixed) - pattern value
* minValue/maxValue - min and max for numeric values or quanities
* maxLength
* constraint
* condition?
* map - if element is key-value map


## Schema of fhirschema

```yaml

resourceType: 'FHIRSchema'
url: 'https://fhirschema.org/element-annotations'
version: '0.0.1'
elements:
  mustSupport: {type: 'boolean'}
  isModifier:  {type: 'boolean'}
  short:       {type: 'string'}

resourceType: 'FHIRSchema'
url: https://fhirschema.org/shared
version: '0.0.1'
elements:
  profiles:   {array: true, type: 'url'}
  required:   {array: true, type: 'code'}
  excluded:   {array: true, type: 'code'}
  choices:    map: true
              elements:
                  elements: {array: true, type: 'code'}
  elements:   {map: true, type: 'https://fhirschema.org/element|0.0.1'}
  constraint:
    map: true
    elements:
      expression: {type: 'fhirpath'}

resourceType: 'FHIRSchema'
url: 'https://fhirschema.org/element'
version: '0.0.1'
profiles:
 - fhirschema.org/shared|0.0.1
 - fhirschema.org/element-annotations|0.0.1
elements:
  type:      {type: 'code'}
  refers:    {array: true, type: 'code'}

  array:    {type: 'boolean'}
  min:      {type: 'integer'}
  max:      {type: 'integer'}

  choiceOf: {type: 'code'}

  enum:     {array: true, type: 'code'}

  slicing:
    elements:
       slices:
         map: true
         elements:
           min: {type: 'integer'}
           max: {type: 'integer'}
           schema: 'https://fhirschema.org/elements'

  binding:
    elements:
       strength: {type: 'code'}
       valueSet: {type: 'url'}
       additional:
         array: true
         elements:
            valueSet: {type: 'url'}

resourceType: 'FHIRSchema'
url: 'https://fhirschema.org/schema'
version: '0.0.1'
profiles:
 - fhirschema.org/shared|0.0.1
elements:
  resourceType:   {type: 'code'}
  kind:           {type: 'code'}
  url:            {type: 'url'}
  version:        {type: 'string'}
  baseDefinition: {type: 'url'}
```
