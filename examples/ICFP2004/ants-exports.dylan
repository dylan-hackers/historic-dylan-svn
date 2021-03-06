module: dylan-user

define library ants
  use common-dylan;
  use io;
  use file-system;
  use regular-expressions;
  use random;

  export ants;
end library ants;

define module ants
  use common-dylan;
  use streams;
  use standard-io;
  use format-out;
  use random;
  use file-system;
  use regular-expressions, exclude: {split};

  export <position>,
    x,
    y,
    make-position,
    <direction>,
    adjacent-cell,
    <left-or-right>,
    turn,
    <sense-direction>,
    sensed-cell,
    <color>,
    other-color,
    <state>,
    <ant>,
    id,
    state,
    color,
    resting,
    direction,
    has-food,
    state-setter,
    resting-setter,
    direction-setter,
    has-food-setter,
    <cell>,
    rocky,
    ant,
    food,
    red-marker,
    black-marker,
    anthill,
    ant-setter,
    food-setter,
    *world*,
    cell-at,
    some-ant-is-at,
    ant-at,
    ant-at-setter,
    set-ant-at,
    clear-ant-at,
    ant-is-alive,
    find-ant,
    kill-ant-at,
    food-at,
    food-at-setter,
    anthill-at,
    read-map,
    <marker>,
    set-marker-at,
    clear-marker-at,
    check-marker-at,
    check-any-marker-at,
    <ant-condition>,
    cell-matches,
    <instruction>,
    <sense>,
    <mark>,
    <unmark>,
    <pickup>,
    <drop>,
    <turn>,
    <move>,
    <flip>,
    *red-brain*,
    *black-brain*,
    get-instruction,
    read-state-machine,
    parse-instruction,
    adjacent-ants,
    check-for-surrounded-ant-at,
    check-for-surrounded-ants,
    step,
    *random-seed*,
    randomint,
    load-world,
    step-world,
    unparse,
    <world>,
    world-x,
    world-y,
    state-success, state-success-setter,
    state-failure, state-failure-setter,
    state-true, state-true-setter,
    state-false, state-false-setter;

end module ants;
