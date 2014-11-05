function getHashValue(key) {
  // http://stackoverflow.com/a/11920807
  return location.hash.match(new RegExp(key+'=([^&]*)'))[1];
}

function setValue(document_id, hash_key) {
  document.getElementById(document_id).innerHTML = getHashValue(hash_key);
}

function extractValues() {
  setValue("access-token", "access_token");
}
