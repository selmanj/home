# Bad hash function, don't use for anything important!
function hash_str() {
	local -r in="$1"
	local -r bucket_size="$2"
	local sum=0
	for (( i=0; i<${#in}; i++ )); do
		local ord="$(printf '%d\n' "'${in:$i:1}")"
		sum=$((sum+ord))
	done
	echo "$((sum % bucket_size))"
}

# Prints a range of integers representing non-ugly colors for colorizing stuff.
# If 256 colors are supported, range is in 256
function color_choices() {
	# Note we ignore ugly/bad colors like black
	if [[ "$(tput colors)" -eq 256 ]]; then 
		echo $(seq 1 15) $(seq 18 231) $(seq 239)
	else
		echo $(seq 1 15)
	fi
}

# Color a hostname for prompt integration (meant for bash)
function color_hostname_for_prompt() {
	local -r hostname="$1"

	local -r colors=($(color_choices))
	local -r hash_val="$(hash_str ${hostname} ${#colors[@]})"
	local -r color=${colors[${hash_val}]}

	echo -e "\001\e[38;5;${color}m\002${hostname}"
}
