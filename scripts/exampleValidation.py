import sys
from jsonschema import validate
import jsonschema
import json
import os

directory = sys.argv[2]
schema = sys.argv[1]

failed_examples = []

with open(schema, 'rb') as sc:
    schema_json = json.load(sc)

for (dir, _dirnames, filenames) in os.walk(directory):
    for file in filenames:
        if file.endswith('.json'):

            absolute_path = os.path.join(dir, file)

            with open(absolute_path, 'rb') as instance:
                instance_json = json.load(instance)

            try:
                validate(instance=instance_json, schema=schema_json)
            except jsonschema.ValidationError as err:
                failed_examples.append(absolute_path)

if len(failed_examples) == 0:
    print("All JSON examples were validated")
    exit(0)
else:
    print("These examples failed the validation check:")
    print("")
    for example in failed_examples:
        print("", example)
    exit(1)